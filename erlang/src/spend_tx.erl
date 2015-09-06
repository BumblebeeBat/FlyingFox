-module(spend_tx).
-export([doit/1]).
-record(spend, {pub = "", from=0, to=0, amount=0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx) ->
    To = block_tree:acc(Tx#spend.to),
    F = block_tree:acc(Tx#spend.from),
    A = Tx#spend.amount,
    NT = #acc{nonce = To#acc.nonce, 
              pub = To#acc.pub, 
              balance = To#acc.balance + A},
    NF = #acc{nonce = F#acc.nonce, 
              pub = F#acc.pub, 
              balance = F#acc.balance - A},
    {[NT, NF], []}.

