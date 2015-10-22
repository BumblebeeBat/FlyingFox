%New transaction type, similar to slasher.
%Each validator needs a minimum amount of money in their account.
%If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
%Otherwise it would be possible to reduce the total number of validators without paying a fee.
-module(repo_tx).
-export([doit/7, repo/3]).
-record(repo, {acc = 0, nonce = 0, target = 0, fee = 0, channels = []}).
repo(Target, Fee, Channels) ->
    Acc = keys:id(),
    A = block_tree:account(Acc),
    T = block_tree:account(Target),
    true = low_balance(T, block_tree:total_coins(), block_tree:height()),
    Nonce = accounts:nonce(A),
    tx_pool:absorb(keys:sign(#repo{acc = Acc, nonce = Nonce + 1, target = Target, fee = Fee, channels = Channels})).
low_balance(Acc, TotalCoins, NewHeight) -> 
    UCost = accounts:unit_cost(Acc, TotalCoins),
    MinBalance = UCost * constants:max_reveal(),
    Gap = NewHeight - accounts:height(Acc),
    Cost = UCost * Gap,
    NewBalance = accounts:balance(Acc) - Cost,
    MinBalance div 2 > NewBalance.
   
all_channels(Amount, _, _, _, _) when Amount < 0 -> 0 = 1;
all_channels(0, _, _, _, _) -> true;
all_channels(_, _, _, _, []) -> false;
all_channels(Amount, Accn, Channels, ParentKey, [Chn|Chs]) -> 
    Channel = block_tree:channel(Chn, ParentKey, Channels),
    N = channels:bal1(Channel) + channels:bal2(Channel),
    Accn = case channels:type(Channel) of
	delegated_1 -> channels:acc1(Channel);
	delegated_2 -> channels:acc2(Channel)
    end,
    all_channels(Amount - N, Accn, Channels, ParentKey, Chs).
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Acc = Tx#repo.acc,
    Target = Tx#repo.target,
    A = block_tree:account(Acc, ParentKey, Accounts),
    T = block_tree:account(Target, ParentKey, Accounts),
    true = low_balance(T, TotalCoins, NewHeight),
    true = all_channels(accounts:delegated(T), Target, Channels, ParentKey, Tx#repo.channels),
    %the Tx also needs to list every channel that they are delegated for, so we can delete those too.
    KT = 3,%deletes (KT - 1) / (KT) of their balance, and gives rest as reward.
    Keep = accounts:balance(T) div KT,
    NA = accounts:update(A, NewHeight, constants:delete_account_reward() + Keep, 0, 1, TotalCoins),
    Accounts2 = dict:store(Acc, NA, Accounts),
    Accounts3 = dict:store(Target, accounts:empty(), Accounts2),
    {Channels, Accounts3, TotalCoins + constants:delete_account_reward() - (Keep * (KT - 1)), S}.
