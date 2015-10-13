-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([next_top/2,doit/6,tc_increases/1,to_channel/4,create_channel/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).

create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When first creating a new channel, don't add the id. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ToAcc = block_tree:account(To),
    true = accounts:balance(Acc) > MyBalance,
    true = accounts:balance(ToAcc) > TheirBalance,
    Tx = #tc{acc1 = Id, acc2 = To, nonce = accounts:nonce(Acc) + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance},
    keys:sign(Tx).
    
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ChannelPointer = block_tree:channel(ChannelId),%[-6,"channel",2,0,3]
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, block_tree:read(top), ChannelId),
    TC = sign:data(SignedToChannel),
    SignedTx = keys:sign(#tc{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, bal1 = TC#tc.bal1 + Inc1, bal2 = TC#tc.bal2 + Inc2, consensus_flag = true, id = ChannelId, fee = Fee, nonce = accounts:nonce(Acc) + 1, increment = Inc1 + Inc2}),
    sign:set_revealed(SignedTx, ChannelId).

next_top(DBroot, Channels) -> next_top_helper(channels:array(), channels:top(), DBroot, Channels).
next_top_helper(Array, Top, DBroot, Channels) ->
    EmptyAcc = #channel{},
    case block_tree:channel(Top, DBroot, Channels) of
	EmptyAcc -> Top;
	_ ->
	    <<A:Top,_:1,B/bitstring>> = Array,
	    NewArray = <<A:Top,1:1,B/bitstring>>,
	    NewTop = channels:walk(Top, NewArray),
	    next_top_helper(NewArray, NewTop, DBroot, Channels)
    end.
doit(SignedTx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    Tx = sign:data(SignedTx),
    NewId = sign:revealed(SignedTx),
    From = Tx#tc.acc1,
    false = From == Tx#tc.acc2,
    Acc1 = block_tree:account(Tx#tc.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#tc.acc2, ParentKey, Accounts),
    ChannelPointer = block_tree:channel(NewId, ParentKey, Channels),
    EmptyChannel = #channel{},
    true = Tx#tc.bal1 > -1,
    true = Tx#tc.bal2 > -1,
    true = is_integer(Tx#tc.bal1),
    true = is_integer(Tx#tc.bal2),
    CF = Tx#tc.consensus_flag,
    if
        ChannelPointer == EmptyChannel ->
                                                %use channel.timeout to store the nonce of the account creator. That way the pointer uniquely points to 1 tx in the block that created it.
            NewId = next_top(ParentKey, Channels),
            Balance1 =  - Tx#tc.bal1 - Tx#tc.fee,
            Balance2 =  - Tx#tc.bal2 - Tx#tc.fee,
            Increment = Tx#tc.bal1 + Tx#tc.bal2,
            Increment = Tx#tc.increment,%err
	    %check if one of the pubkeys is keys:pubkey().
	    %If so, then add it to the mychannels module.
            1=1;
        true ->
            NewId = Tx#tc.id,
            SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, NewId),
            OriginTx = sign:data(SignedOriginTx),
            CF = OriginTx#tc.consensus_flag,
            AccN1 = OriginTx#tc.acc1,
            AccN1 = Tx#tc.acc1,
            AccN2 = OriginTx#tc.acc2,
            AccN2 = Tx#tc.acc2,
            OldVol = OriginTx#tc.bal1 + OriginTx#tc.bal2,
            NewVol = Tx#tc.bal2 + Tx#tc.bal1,
            Increment = NewVol - OldVol,
            Increment = Tx#tc.increment,
            true = (-1 < Increment),%to_channel can only be used to increase the amount of money in a channel, for consensus reasons. 
            Balance1 = - Tx#tc.bal1 + OriginTx#tc.bal1 - Tx#tc.fee,
            Balance2 = - Tx#tc.bal2 + OriginTx#tc.bal2 - Tx#tc.fee,
            1=1
    end,
    Nonce = accounts:nonce(Acc1),
    Nonce = Tx#tc.nonce - 1,
    if
        CF ->
            D1 = Increment,
            D2 = 0;
        true ->
            D1 = 0,
            D2 = Increment
    end,
    %update height in each account, and have them pay fees for delegation.
    N1 = accounts:update(Acc1, NewHeight, Balance1, D1, 1),
    N2 = accounts:update(Acc2, NewHeight, Balance2, D2, 0),
    true = NewId < constants:max_channel(),
    MyKey = keys:pubkey(),
    APub1 = accounts:pub(Acc1),
    APub2 = accounts:pub(Acc2),
    if
	(ChannelPointer == EmptyChannel and ((APub1 == MyKey) or (APub2 == MyKey))) -> my_channels:add(NewId);
	true -> 1=1
    end,
    Ch = #channel{tc = NewHeight, creator = Tx#tc.acc1, timeout = Tx#tc.nonce},
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId, Ch, Channels),
    {NewChannels, NewAccounts, TotalCoins}.
tc_increases(NewNumber) ->
    ParentKey = block_tree:read(top),
    CF = constants:finality(),
    if
        NewNumber < CF -> 0;
        true -> 
            FBlock = block_tree:read_int(NewNumber - CF, ParentKey),
            tc_increases(block_tree:txs(FBlock), 0)
    end.
%filter out tc type txs. add up the amount of money in each channel. Exclude channels that aren't still open.
tc_increases([], X) -> X;
tc_increases([SignedTx|T], X) ->
    Tx = sign:data(SignedTx),
    if
	is_record(Tx, tc) ->
	    A = Tx#tc.increment,
	    Id = Tx#tc.id,
	    ChannelPointer = block_tree:channel(Id, dict:new()),
	    ParentKey = block_tree:read(top),
	    SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, channel_block:id(Tx)),
	    OriginTx = sign:data(SignedOriginTx),
	    if 
		not is_record(OriginTx, tc) -> tc_increases(T, X);
		(Tx#tc.acc1 == OriginTx#tc.acc1) and (Tx#tc.acc2 == OriginTx#tc.acc2) ->
		    tc_increases(T, X+A);
		true -> tc_increases(T, X)
	    end;
	true -> tc_increases(T, X)
    end.
