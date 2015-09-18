-module(create_channel_tx).
-export([doit/4]).
-record(channel, {delay = 0, bal1 = 0, bal2 = 0, consensus_flag = false, acc1 = 0, acc2 = 0, creationBlockNumber = 0}).
-record(cc, {acc1 = 0, acc2 = 1, delay = 10, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx, ParentKey, Accounts, Channels) ->
    From = Tx#cc.acc1,
    false = From == Tx#cc.acc2,
    Ch = block_tree:channel(Tx#cc.id, ParentKey, Channels),
    Ch = #channel{},%You can only fill space in the database that are empty.
    true = Tx#cc.id < constants:max_channel(),
    OneUnder = block_tree:channel(Tx#cc.id-1, ParentKey, Accounts),
    false = ((OneUnder == #channel{}) and (not (Tx#cc.id == 0))),%You can only fill a space if the space below you is already filled.
    Acc1 = block_tree:account(Tx#cc.acc1, ParentKey, Accounts),
    %_Acc2 = block_tree:account(Tx#cc.acc2, ParentKey, Accounts),
    Creation = block_tree:height(ParentKey) + 1,
    N1 = #acc{balance = Acc1#acc.balance - Tx#cc.bal1 - Tx#cc.bal2 - constants:create_channel_fee(),
	      nonce = Acc1#acc.nonce + 1, 
	      pub = Acc1#acc.pub},
    Channel = #channel{delay = Tx#cc.delay,
		       bal1 = Tx#cc.bal1,
		       bal2 = Tx#cc.bal2,
		       consensus_flag = Tx#cc.consensus_flag,
		       acc1 = Tx#cc.acc1,
		       acc2 = Tx#cc.acc2,
		       creationBlockNumber = Creation},
    Accounts2 = dict:store(Tx#cc.acc1, N1, Accounts),
    Channels2 = dict:store(Tx#cc.id, Channel, Channels),
    {Accounts2, Channels2}.
