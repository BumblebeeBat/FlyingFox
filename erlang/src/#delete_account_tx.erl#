-module(delete_account_tx).
-export([doit/4]).
-record(da, {from = 0, nonce = 0, to = <<"0">>}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx, ParentKey, Accounts, Channels) ->
    F = block_tree:account(Tx#da.from, ParentKey, Accounts),
    To = block_tree:account(Tx#da.to, ParentKey, Accounts),
    NT = #acc{nonce = 0,
              pub = Tx#da.to,
              balance = To#acc.balance + F#acc.balance + constants:delete_account_reward()},
    NF = #acc{nonce = 0,
              pub = 0,
              balance = 0},
    Nonce = F#acc.nonce + 1,
    Nonce = Tx#da.nonce,
    true = NT#acc.balance > 0,
    Accounts2 = dict:store(Tx#da.to, NT, Accounts),
    Accounts3 = dict:store(Tx#da.from, NF, Accounts2),
    {Accounts3, Channels}.

