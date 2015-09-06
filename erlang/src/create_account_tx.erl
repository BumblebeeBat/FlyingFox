-module(create_account_tx).
-export([doit/1]).
-record(create_account, {pub = "", from=0, to=0, amount=0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx) ->
    %To = #acc{balance},%block_tree:acc(Tx#spend.to),
    F = block_tree:acc(Tx#create_account.from),
    A = Tx#create_account.amount,
    NT = #acc{nonce = 0,
              pub = Tx#create_account.pub, 
              balance = A},
    NF = #acc{nonce = F#acc.nonce, 
              pub = F#acc.pub, 
              balance = F#acc.balance - A - constants:account_fee()},
    {[NT, NF], []}.

