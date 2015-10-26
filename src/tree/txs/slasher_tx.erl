-module(slasher_tx).
-export([doit/7, slasher/2]).
-record(slasher_tx, {acc = 0, nonce = 0, sign_tx = 0}).
slasher(Acc, SignTx) -> 
    A = block_tree:account(Acc),
    #slasher_tx{acc = Acc, nonce = accounts:nonce(A) + 1, sign_tx = SignTx}.
    
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, Secrets, NewHeight) ->
    %check that the sign_tx was actually signed by the right person...
    %prove that a validator double-signed.
    %take some of their deposit, delete the rest of deposit.
    ST = sign:data(Tx#slasher_tx.sign_tx),
    H = sign_tx:number(ST)+1,
    A = sign_tx:acc(ST),
    true = H > -1,
    Hgap = NewHeight - H,
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = reveal:origin_tx(OriginTxs, A),
    %io:fwrite(packer:pack(OriginBlock)),
    WL = sign_tx:winners_length(OriginTx),
    SH = sign_tx:secret_hash(OriginTx),
    Number = sign_tx:number(OriginTx),
    Secret = block_tree:secret(Number, SH, ParentKey, Secrets),
    Secret = true,

    N = sign_tx:number(ST),
    %SH = sign_tx:secret_hash(Tx#slasher_tx.sign_tx),
    %WL = sign_tx:winners_length(Tx#slasher_tx.sign_tx),
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    Power = block_tree:power(block_tree:block(ParentKey)),
    DReward = fractions:multiply_int(constants:delegation_fee(), Power) div constants:validators_elected_per_block(),%half gets deleted, half is a reward.
    TReward = fractions:multiply_int(constants:slasher_reward(), ((Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL)) div 2,%???
    Acc = block_tree:account(Tx#slasher_tx.acc, ParentKey, Accounts),
    NN = accounts:update(Acc, NewHeight, TReward, 0, 1, TotalCoins),
    Nonce = accounts:nonce(NN),
    Nonce = Tx#slasher_tx.nonce,
    NewAccounts = dict:store(Tx#slasher_tx.acc, NN, Accounts),
    NewSecrets = dict:store({N, SH}, false, Secrets),
    {Channels, NewAccounts, TotalCoins - TReward, NewSecrets}.

