-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([next_top/2,doit/7,tc_increases/1,to_channel/4,create_channel/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = delegated_1, fee = 0, id = -1, increment = 0}).
create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When creating a new channel, you don't choose your own ID for the new channel. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ToAcc = block_tree:account(To),
    true = accounts:balance(Acc) > MyBalance,
    true = accounts:balance(ToAcc) > TheirBalance,
    S = ConsensusFlag,
    true = ((S == delegated_1) or (S == delegated_2)),
    Tx = #tc{acc1 = Id, acc2 = To, nonce = accounts:nonce(Acc) + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance},
    keys:sign(Tx).
    
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Channel = block_tree:channel(ChannelId),
    S = channels:type(Channel),
    true = ((S == delegated_1) or (S == delegated_2)),
    keys:sign(#tc{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), bal1 = channels:bal1(Channel) + Inc1, bal2 = channels:bal2(Channel) + Inc2, consensus_flag = S, id = ChannelId, fee = Fee, nonce = accounts:nonce(Acc) + 1, increment = Inc1 + Inc2}).
%sign:set_revealed(SignedTx, ChannelId).

next_top(DBroot, Channels) -> next_top_helper(channels:array(), channels:top(), DBroot, Channels).
next_top_helper(Array, Top, DBroot, Channels) ->
    EmptyAcc = channels:empty(),
    case block_tree:channel(Top, DBroot, Channels) of
	EmptyAcc -> Top;
	_ ->
	    <<A:Top,_:1,B/bitstring>> = Array,
	    NewArray = <<A:Top,1:1,B/bitstring>>,
	    NewTop = channels:walk(Top, NewArray),
	    next_top_helper(NewArray, NewTop, DBroot, Channels)
    end.
doit(SignedTx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Tx = sign:data(SignedTx),
    if
	Tx#tc.id == -1 -> NewId = sign:revealed(SignedTx);
	true -> NewId = Tx#tc.id
    end,
    From = Tx#tc.acc1,
    false = From == Tx#tc.acc2,
    Channel = block_tree:channel(NewId, ParentKey, Channels),
    Acc1 = block_tree:account(Tx#tc.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#tc.acc2, ParentKey, Accounts),
    EmptyChannel = channels:empty(),
    true = Tx#tc.bal1 > -1,
    true = Tx#tc.bal2 > -1,
    true = is_integer(Tx#tc.bal1),
    true = is_integer(Tx#tc.bal2),
    if
        Channel == EmptyChannel ->
            NewId = next_top(ParentKey, Channels),
            Balance1 =  - Tx#tc.bal1 - Tx#tc.fee,
            Balance2 =  - Tx#tc.bal2 - Tx#tc.fee,
            Increment = Tx#tc.bal1 + Tx#tc.bal2,
            Increment = Tx#tc.increment,
            true = Increment > (TotalCoins div constants:max_channel()),
	    Type = Tx#tc.consensus_flag,
	    true = ((Type == delegated_1) or (Type == delegated_2)),
	    %check if one of the pubkeys is keys:pubkey().
            %If so, then add it to the mychannels module.
	    1=1;
	true ->
	    Type = channels:type(Channel),
	    NewId = Tx#tc.id,
	    AccN1 = channels:acc1(Channel),
	    AccN1 = Tx#tc.acc1,
	    AccN2 = channels:acc2(Channel),
	    AccN2 = Tx#tc.acc2,
	    OldVol = channels:bal1(Channel) + channels:bal2(Channel),
	    NewVol = Tx#tc.bal1 + Tx#tc.bal2,
	    Increment = NewVol - OldVol,
	    Increment = Tx#tc.increment,
            true = (-1 < Increment),%to_channel can only be used to increase the amount of money in a channel, for consensus reasons. 
            Balance1 = - Tx#tc.bal1 + channels:bal1(Channel) - Tx#tc.fee,
            Balance2 = - Tx#tc.bal2 + channels:bal2(Channel) - Tx#tc.fee
    end,
    Nonce = accounts:nonce(Acc1),
    Nonce = Tx#tc.nonce - 1,
    case Tx#tc.consensus_flag of
	delegated_1 -> 
	    D1 = Increment,
	    D2 = 0;
	delegated_2 -> 
	    D1 = 0,
	    D2 = Increment
    end,
    N1 = accounts:update(Acc1, NewHeight, Balance1, D1, 1, TotalCoins),
    N2 = accounts:update(Acc2, NewHeight, Balance2, D2, 0, TotalCoins),
    true = NewId < constants:max_channel(),
    MyKey = keys:pubkey(),
    APub1 = accounts:pub(Acc1),
    APub2 = accounts:pub(Acc2),
    if
	(Channel == EmptyChannel and ((APub1 == MyKey) or (APub2 == MyKey))) -> my_channels:add(NewId);
	true -> 1=1
    end,
    Ch = channels:new(Tx#tc.acc1, Tx#tc.acc2, Tx#tc.bal1, Tx#tc.bal2, Type),
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId, Ch, Channels),
    {NewChannels, NewAccounts, TotalCoins, S}.
tc_increases(Txs) -> tc_increases(Txs, 0).
tc_increases([], X) -> X;
tc_increases([SignedTx|T], X) -> 
    Tx = sign:data(SignedTx),
    case element(1, Tx) of
	tc -> tc_increases(T, X+Tx#tc.increment);
	_ -> tc_increases(T, X)
    end.
%tc_increases(NewNumber, ParentKey) ->
%    CF = constants:finality(),
%    if
%        NewNumber < CF + 1 -> 0;
%        true -> 
%            FBlock = block_tree:read_int(NewNumber - CF, ParentKey),
%            tc_increases(block_tree:txs(FBlock), ParentKey, 0)
%    end.
%tc_increases([], _, X) -> X;
%tc_increases([SignedTx|T], ParentKey, X) ->
    %there is a case where the block increases in power, but the account does not...
    %if the same pair of accounts closes and opens the same channel, in a single block, it shouldn't break anything.
%Tx = sign:data(SignedTx),
%if
%is_record(Tx, tc) ->
%Channel = block_tree:channel(sign:revealed(SignedTx), ParentKey, dict:new()),
%Acc1 = channels:acc1(Channel),
%Acc2 = channels:acc2(Channel),
%if
%(Tx#tc.acc1 == Acc1) and (Tx#tc.acc2 == Acc2) -> tc_increases(T, ParentKey, X+Tx#tc.increment);%this check is not good enough. It is possible to attack my making the power tag on the block incorrect.
%true -> tc_increases(T, ParentKey, X)
%end;
%true -> tc_increases(T, ParentKey, X)
%end.
