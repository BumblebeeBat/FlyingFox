-module(spend_tx).
-export([doit/6, spend/2]).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0, fee = 0}).
spend(To, Amount) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Tx = #spend{from = Id, nonce = accounts:nonce(Acc) + 1, to = To, amount = Amount},
    tx_pool:absorb(keys:sign(Tx)).
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    From = Tx#spend.from,
    false = From == Tx#spend.to,
    To = block_tree:account(Tx#spend.to, ParentKey, Accounts),
    F = block_tree:account(Tx#spend.from, ParentKey, Accounts),
    A = Tx#spend.amount,
    NT = accounts:update(To, NewHeight, A, 0, 0),
    NF = accounts:update(F, NewHeight, -A - Tx#spend.fee, 0, 1),
    %should subtract a fee too. The fee goes to the block creator.
    Nonce = accounts:nonce(NF),
    Nonce = Tx#spend.nonce,
    Accounts2 = dict:store(Tx#spend.to, NT, Accounts),
    {Channels, dict:store(Tx#spend.from, NF, Accounts2), TotalCoins}.

