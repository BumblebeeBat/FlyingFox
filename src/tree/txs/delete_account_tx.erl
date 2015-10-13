-module(delete_account_tx).
-export([doit/6]).
-record(da, {from = 0, nonce = 0, to = <<"0">>}).
%there needs to be a way to include a fee, or else they wont be able to get their tx included.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    F = block_tree:account(Tx#da.from, ParentKey, Accounts),
    To = block_tree:account(Tx#da.to, ParentKey, Accounts),
    NT = accounts:update(To, NewHeight, accounts:balance(F) + constants:delete_account_reward(), 0, 0, TotalCoins),
    Nonce = accounts:nonce(F) + 1,
    Nonce = Tx#da.nonce,
    Accounts2 = dict:store(Tx#da.to, NT, Accounts),
    Accounts3 = dict:store(Tx#da.from, accounts:empty(), Accounts2),
    {Channels, Accounts3, TotalCoins + constants:delete_account_reward() - constants:create_account_fee()}.

