-module(slasher_tx).
-export([doit/7]).
-record(slasher_tx, {acc = 0, nonce = 0, sign_tx = 0}).

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, Secrets, NewHeight) ->
    %prove that a validator double-signed.
    %take some of their deposit, delete the rest of deposit.
    H = sign_tx:number(Tx#slasher_tx.sign_tx),
    true = H > 1,
    Hgap = NewHeight - H,
    true = Hgap > constants:min_reveal(),
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = reveal:origin_tx(OriginTxs, Tx#slasher_tx.acc),
    WL = sign_tx:winners_length(OriginTx),
    SH = sign_tx:secret_hash(OriginTx),
    Number = sign_tx:number(OriginTx),
    Secret = block_tree:secret(Number, SH, ParentKey, Secrets),
    Secret = true,
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    Power = block_tree:power(block_tree:block(ParentKey)),
    DReward = fractions:multiply_int(constants:delegation_reward(), Power) div constants:maximum_validators_per_block(),
    TReward = fractions:multiply_int(constants:slasher_reward(), ((Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL)),
    N = accounts:update(Tx#slasher_tx.acc, NewHeight, TReward, 0, 1, TotalCoins),
    
    NewAccounts = dict:store(Tx#slasher_tx.acc, N, Accounts),
    NewSecrets = dict:store({Number, SH}, false, Secrets),
    {Channels, NewAccounts, TotalCoins + TReward, NewSecrets}.

