%New transaction type, similar to slasher.
%Each validator needs a minimum amount of money in their account.
%If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
%Otherwise it would be possible to reduce the total number of validators without paying a fee.
-module(repo_tx).
-export([doit/7, repo/3]).
-record(repo, {from = 0, nonce = 0, target = 0, fee = 0}).
low_balance(Acc) -> false.
repo(Acc, Target, Fee) ->
    A = block_tree:account(Acc),
    T = block_tree:account(Target),
    true = low_balance(T),
    Nonce = accounts:nonce(A),
    #repo{from = Acc, nonce = Nonce + 1, target = Target, fee = Fee}.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    {Channels, Accounts, TotalCoins + constants:delete_account_reward(), S}.
