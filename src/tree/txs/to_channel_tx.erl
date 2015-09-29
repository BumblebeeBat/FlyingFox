-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([doit/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce1 = 0, nonce2 = 0, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx, ParentKey, Channels, Accounts, BlockGap) ->
    From = Tx#tc.acc1,
    false = From == Tx#tc.acc2,
    Acc1 = block_tree:account(Tx#tc.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#tc.acc2, ParentKey, Accounts),
    ChannelPointer = block_tree:channel(Tx#tc.id, ParentKey, Channels),
    EmptyChannel = #channel{},
    Nonce1 = Acc1#acc.nonce + 1,
    Nonce2 = Acc2#acc.nonce + 1,
    Nonce1 = Tx#tc.nonce1,
    Nonce2 = Tx#tc.nonce2,
    if% the space isn't empty, then we don't necessarily crash. If the same pair of addresses want to increment their balances, we should let them. 
        ChannelPointer == EmptyChannel ->
            OneBelow = block_tree:channel(Tx#tc.id-1, ParentKey, Channels),%You can only fill a space if the space below you is already filled.
            if
                Tx#tc.id == 1 -> 1=1;
                true -> false = EmptyChannel == OneBelow
            end,
	    Balance1 = Acc1#acc.balance - Tx#tc.bal1 - Tx#tc.fee,
	    Balance2 = Acc2#acc.balance - Tx#tc.bal2 - Tx#tc.fee,
            1=1;
        true ->
            OriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#tc.id),
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
    true = Tx#tc.id < constants:max_channel(),
    true = N1#acc.balance > 0,
    true = N2#acc.balance > 0,
    T = block_tree:read(top),
    Top = block_tree:height(T),
    Ch = #channel{tc = Top + BlockGap, creator = Tx#tc.acc1},
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(Tx#tc.id, Ch, Channels),
    {NewChannels, NewAccounts}.
