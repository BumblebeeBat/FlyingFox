-module(spend_tx).
-export([doit/4]).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
doit(Tx, ParentKey, Channels, Accounts) ->
    From = Tx#spend.from,
    false = From == Tx#spend.to,
    To = block_tree:account(Tx#spend.to, ParentKey, Accounts),
    F = block_tree:account(Tx#spend.from, ParentKey, Accounts),
    A = Tx#spend.amount,
    NT = #acc{nonce = To#acc.nonce, 
              pub = To#acc.pub, 
              balance = To#acc.balance + A},
    NF = #acc{nonce = F#acc.nonce + 1, 
              pub = F#acc.pub, 
              balance = F#acc.balance - A},
    %should subtract a fee too. The fee goes to the block creator.
    Nonce = NF#acc.nonce,
    Nonce = Tx#spend.nonce,
    true = NT#acc.balance > 0,
    true = NF#acc.balance > 0,
    Accounts2 = dict:store(Tx#spend.to, NT, Accounts),
    {Channels, dict:store(Tx#spend.from, NF, Accounts2)}.

