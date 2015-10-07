-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([next_top/2,doit/5,tc_increases/1,to_channel/4,create_channel/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).

create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When first creating a new channel, don't add the id. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ToAcc = block_tree:account(To),
    true = Acc#acc.balance > MyBalance,
    true = ToAcc#acc.balance > TheirBalance,
    Tx = #tc{acc1 = Id, acc2 = To, nonce = Acc#acc.nonce + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance},
    keys:sign(Tx).
    
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ChannelPointer = block_tree:channel(ChannelId),%[-6,"channel",2,0,3]
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, block_tree:read(top), ChannelId),
    TC = SignedToChannel#signed.data,
    SignedTx = keys:sign(#tc{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, bal1 = TC#tc.bal1 + Inc1, bal2 = TC#tc.bal2 + Inc2, consensus_flag = true, id = ChannelId, fee = Fee, nonce = Acc#acc.nonce + 1, increment = Inc1 + Inc2}),
    #signed{data = SignedTx#signed.data, sig2 = SignedTx#signed.sig2, sig = SignedTx#signed.sig, revealed = ChannelId}.

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
doit(SignedTx, ParentKey, Channels, Accounts, BlockGap) ->
    Tx = SignedTx#signed.data,
    NewId = SignedTx#signed.revealed,
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
            Balance1 = Acc1#acc.balance - Tx#tc.bal1 - Tx#tc.fee,
            Balance2 = Acc2#acc.balance - Tx#tc.bal2 - Tx#tc.fee,
            Increment = Tx#tc.bal1 + Tx#tc.bal2,
            Increment = Tx#tc.increment,%err
	    %check if one of the pubkeys is keys:pubkey().
	    %If so, then add it to the mychannels module.
            1=1;
        true ->
            NewId = Tx#tc.id,
            SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, NewId),
            OriginTx = SignedOriginTx#signed.data,
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
            Balance1 = Acc1#acc.balance - Tx#tc.bal1 + OriginTx#tc.bal1 - Tx#tc.fee,
            Balance2 = Acc2#acc.balance - Tx#tc.bal2 + OriginTx#tc.bal2 - Tx#tc.fee,
            1=1
    end,
    Nonce = Acc1#acc.nonce + 1,
    Nonce = Tx#tc.nonce,
    if
        CF ->
            D1 = Increment,
            D2 = 0;
        true ->
            D1 = 0,
            D2 = Increment
    end,
    N1 = #acc{balance = Balance1,
              nonce = Nonce,
              pub = Acc1#acc.pub,
              delegated = Acc1#acc.delegated + D1},
    N2 = #acc{balance = Balance2,
              nonce = Acc2#acc.nonce,
              pub = Acc2#acc.pub,
              delegated = Acc2#acc.delegated + D2},
    true = NewId < constants:max_channel(),
    true = N1#acc.balance > 0,
    true = N2#acc.balance > 0,
    MyKey = keys:pubkey(),
    if
	(ChannelPointer == EmptyChannel and ((Acc1#acc.pub == MyKey) or (Acc2#acc.pub == MyKey))) -> my_channels:add(NewId);
	true -> 1=1
    end,
    T = block_tree:read(top),
    Top = block_tree:height(T),
    Ch = #channel{tc = Top + BlockGap, creator = Tx#tc.acc1, timeout = Tx#tc.nonce},
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId, Ch, Channels),
    {NewChannels, NewAccounts}.
tc_increases(NewNumber) ->
    ParentKey = block_tree:read(top),
    CF = constants:finality(),
    if
        NewNumber < CF -> TcIncreases = 0;
        true -> 
            FBlock = block_tree:read_int(NewNumber - CF, ParentKey),
            TcIncreases = tc_increases(block_tree:txs(FBlock), 0)
    end,
    TcIncreases.
%filter out tc type txs. add up the amount of money in each channel. Exclude channels that aren't still open.
tc_increases([], X) -> X;
tc_increases([#signed{data = Tx}|T], X) when is_record(Tx, tc) ->
    A = Tx#tc.increment,
    Id = Tx#tc.id,
    ChannelPointer = block_tree:channel(Id, dict:new()),
    ParentKey = block_tree:read(top),
    SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, channel_block:id(Tx)),
    OriginTx = SignedOriginTx#signed.data,
    if
        (Tx#tc.acc1 == OriginTx#tc.acc1) and
        (Tx#tc.acc2 == OriginTx#tc.acc2) ->
            SA = A;
        true -> SA = 0
    end,
    
    tc_increases(T, X+SA).
