-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([doit/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce1 = 0, nonce2 = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).

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
    if% the space isn't empty, then we don't necessarily crash. If the same pair of addresses want to increment their balances, we should let them. 
	ChannelPointer == EmptyChannel ->
	    Nonce1 = Acc1#acc.nonce + 1,
	    Nonce2 = Acc2#acc.nonce + 1,
	    Nonce1 = Tx#tc.nonce1,
	    Nonce2 = Tx#tc.nonce2,
	    NewId = next_top(ParentKey, Channels),
	    Balance1 = Acc1#acc.balance - Tx#tc.bal1 - Tx#tc.fee,
	    Balance2 = Acc2#acc.balance - Tx#tc.bal2 - Tx#tc.fee,
	    %check if one of the pubkeys is keys:pubkey().
	    %If so, then add it to the mychannels module.
            1=1;
        true ->
	    Nonce1 = Acc1#acc.nonce,
	    Nonce2 = Acc2#acc.nonce,
	    NewId = Tx#tc.id,
            SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, NewId),
	    OriginTx = SignedOriginTx#signed.data,
            AccN1 = OriginTx#tc.acc1,
            AccN1 = Tx#tc.acc1,
            AccN2 = OriginTx#tc.acc2,
            AccN2 = Tx#tc.acc2,
            true = OriginTx#tc.bal1 < Tx#tc.bal1 + 1,%to_channel can only be used to increase the amount of money in a channel, for consensus reasons. (maybe if consensus flag is off, we should let them decrease too?)
            true = OriginTx#tc.bal2 < Tx#tc.bal2 + 1,
	    Balance1 = Acc1#acc.balance - Tx#tc.bal1 + OriginTx#tc.bal1 - Tx#tc.fee,
	    Balance2 = Acc2#acc.balance - Tx#tc.bal2 + OriginTx#tc.bal2 - Tx#tc.fee,
            1=1
    end,
    N1 = #acc{balance = Balance1,
	      nonce = Nonce1,
	      pub = Acc1#acc.pub},
    N2 = #acc{balance = Balance2,
	      nonce = Nonce2,
	      pub = Acc2#acc.pub},
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
    Ch = #channel{tc = Top + BlockGap, creator = Tx#tc.acc1},
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId, Ch, Channels),
    {NewChannels, NewAccounts}.
