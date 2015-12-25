-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([next_top/2,doit/7,tc_increases/1,to_channel/4,create_channel/5,my_side/1,min_ratio/2,test/0,grow_ratio/2]).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = <<"delegated_1">>, fee = 0, id = -1, increment = 0}).
my_side(Tx) ->
    A1 = Tx#tc.acc1,
    B1 = Tx#tc.bal1,
    A2 = Tx#tc.acc2,
    B2 = Tx#tc.bal2,
    case keys:id() of
	A1 -> B1;
	A2 -> B2
    end.
%half_them(Tx) ->
%    A1 = Tx#tc.acc1,
%    A2 = Tx#tc.acc2,
%    B = Tx#tc.bal1 < Tx#tc.bal2,
%    case keys:id() of
%	A1 -> B;
%	A2 -> (not B)
%    end.
good_key(S) -> ((S == <<"delegated_1">>) or (S == <<"delegated_2">>)).
create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When creating a new channel, you don't choose your own ID for the new channel. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ToAcc = block_tree:account(To),
    true = accounts:balance(Acc) > MyBalance,
    true = accounts:balance(ToAcc) > TheirBalance,
    S = ConsensusFlag,
    true = good_key(S),
    Tx = #tc{acc1 = Id, acc2 = To, nonce = accounts:nonce(Acc) + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance},
    keys:sign(Tx).
    
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Channel = block_tree:channel(ChannelId),
    S = channels:type(Channel),
    true = good_key(S),
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
            NewId2 = next_top(ParentKey, Channels),
            Balance1 =  - Tx#tc.bal1 - Tx#tc.fee,
            Balance2 =  - Tx#tc.bal2 - Tx#tc.fee,
            Increment = Tx#tc.bal1 + Tx#tc.bal2,
            Increment = Tx#tc.increment,
            true = Increment > (TotalCoins div constants:max_channel()),
            Type = Tx#tc.consensus_flag,
            true = good_key(Type),
            %check if one of the pubkeys is keys:pubkey().
            %If so, then add it to the mychannels module.
            1=1;
        true ->
	    Type = channels:type(Channel),
	    NewId2 = Tx#tc.id,
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
	<<"delegated_1">> -> 
	    D1 = Increment,
	    D2 = 0;
	<<"delegated_2">> -> 
	    D1 = 0,
	    D2 = Increment
    end,
    N1 = accounts:update(Acc1, NewHeight, Balance1, D1, 1, TotalCoins),
    N2 = accounts:update(Acc2, NewHeight, Balance2, D2, 0, TotalCoins),
    true = NewId2 < constants:max_channel(),
    MyKey = keys:pubkey(),
    APub1 = accounts:pub(Acc1),
    APub2 = accounts:pub(Acc2),
    Ch = channels:new(Tx#tc.acc1, Tx#tc.acc2, Tx#tc.bal1, Tx#tc.bal2, Type),
    if
	((Channel == EmptyChannel) and ((APub1 == MyKey) or (APub2 == MyKey))) -> channel_manager:new_channel(NewId2, Ch);
	true -> 1=1
    end,
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId2, Ch, Channels),
    {NewChannels, NewAccounts, TotalCoins, S}.
tc_increases(Txs) -> tc_increases(Txs, 0).
tc_increases([], X) -> X;
tc_increases([SignedTx|T], X) -> 
    Tx = sign:data(SignedTx),
    case element(1, Tx) of
	tc -> tc_increases(T, X+Tx#tc.increment);
	_ -> tc_increases(T, X)
    end.

min_ratio(Max, Tx) ->%This works for new channels. We need a seperate one for adding funds to an existing channel.
    B1 = Tx#tc.bal1,
    B2 = Tx#tc.bal2,
    B = B2 + B1,
    ratio_helper(B1, B2, B, Max, Tx).
ratio_helper(B1, B2, B, Max, Tx) ->
    A1 = Tx#tc.acc1,
    A2 = Tx#tc.acc2,
    F = case keys:id() of
	A1 -> fractions:new(B1, B);
	A2 -> fractions:new(B2, B)
    end,
    (not fractions:less_than(Max, F)).
grow_ratio(Max, Tx) ->
    Channel = block_tree:channel(Tx#tc.id, tx_pool:channels()),
    I1 = Tx#tc.bal1 - channels:bal1(Channel),
    I2 = Tx#tc.bal2 - channels:bal2(Channel),
    I = I1 + I2,
    ratio_helper(I1, I2, I, Max, Tx).

test() ->
    TxThem = #tc{bal1 = 0, bal2 = 10},
    TxMe = #tc{bal1 = 10, bal2 = 0},
    TxThema = #tc{bal1 = 1, bal2 = 10},
    TxMea = #tc{bal1 = 10, bal2 = 1},
    TxHalf = #tc{bal1 = 10, bal2 = 10},
    Tx = #tc{acc1 = 1, acc2 = 0,nonce = 1,bal1 = 1120000,bal2 = 1100000,fee = 50,id = -1, increment = 2220000},
    true = min_ratio(fractions:new(2, 3), Tx),
    true = min_ratio(fractions:new(1, 2), TxThem),
    false = min_ratio(fractions:new(1, 2), TxMe),
    true = min_ratio(fractions:new(1, 2), TxThema),
    false = min_ratio(fractions:new(1, 2), TxMea),
    true = min_ratio(fractions:new(1, 2), TxHalf),
    success.
  
