-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([doit/5]).
-record(tc, {acc1 = 0, acc2 = 1, nonce1 = 0, nonce2 = 0, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
-record(channel, {height = 0, creator = 0, nonce = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx, ParentKey, Channels, Accounts, BlockGap) ->
    From = Tx#tc.acc1,
    false = From == Tx#tc.acc2,
    Acc1 = block_tree:account(Tx#tc.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#tc.acc2, ParentKey, Accounts),
    Channel = block_tree:channel(Tx#tc.id, ParentKey, Channels),
    EmptyChannel = #channel{},
    if% the space isn't empty, then we don't necessarily crash. If the same pair of addresses want to increment their balances, we should let them. 
	Channel == EmptyChannel ->
	    OneBelow = block_tree:channel(Tx#tc.id-1, ParentKey, Channels),%You can only fill a space if the space below you is already filled.
	    if
		Tx#tc.id == 0 -> 1=1;
		true -> EmptyChannel = OneBelow
	    end,
	    %make sure nonces match tx, and incement nonces.
	    Nonce1 = Acc1#acc.nonce + 1,
	    Nonce2 = Acc2#acc.nonce + 1,
	    Nonce1 = Tx#tc.nonce1,
	    Nonce2 = Tx#tc.nonce2,
	    1=1;
	true ->
	    %make sure Accn in Channel match the tx.
	    Nonce1 = Acc1#acc.nonce,
	    Nonce2 = Acc2#acc.nonce,
	    1=1
    end,
    true = Tx#tc.id < constants:max_channel(),
    N1 = #acc{balance = Acc1#acc.balance - Tx#tc.bal1 - Tx#tc.fee,
	      nonce = Nonce1, 
	      pub = Acc1#acc.pub},
    N2 = #acc{balance = Acc2#acc.balance - Tx#tc.bal2 - Tx#tc.fee,
	      nonce = Nonce2, 
	      pub = Acc2#acc.pub},
    true = N1#acc.balance > 0,
    true = N2#acc.balance > 0,
    T = block_tree:read(top),
    Top = block_tree:height(T),
    Ch = #channel{height = Top + BlockGap, creator = Tx#tc.acc1},
    NewAccounts = dict:store(Tx#tc.acc1, N1, Accounts),
    NewChannels = dict:store(Tx#tc.id, Ch, Channels),
    {NewChannels, NewAccounts}.
