-module(delete_account_tx).
-export([doit/3]).
-record(da, {from = 0, nonce = 0, to = <<"0">>}).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
doit(Tx, ParentKey, Accounts) ->
    F = block_tree:account(Tx#da.from, ParentKey, Accounts),
    To = block_tree:account(Tx#da.to, ParentKey, Accounts),
    NT = #acc{nonce = 0,
              pub = Tx#da.to,
              balance = To#acc.balance + F#acc.balance + constants:delete_account_reward(),
              delegated = To#acc.delegated},
    NF = #acc{nonce = 0,
              pub = 0,
              balance = 0,
              delegated = 0},
    Nonce = F#acc.nonce + 1,
    Nonce = Tx#da.nonce,
    true = NT#acc.balance > 0,
    Accounts2 = dict:store(Tx#da.to, NT, Accounts),
    dict:store(Tx#da.from, NF, Accounts2).

