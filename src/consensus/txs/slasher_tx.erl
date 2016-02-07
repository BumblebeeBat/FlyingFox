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
    io:fwrite("slasher tx 0"),
    ST = sign:data(Tx#slasher_tx.sign_tx),
    H = sign_tx:number(ST)+1,
    A = sign_tx:acc(ST),
    true = H > -1,
    Hgap = NewHeight - H,
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H, ParentKey),
    io:fwrite("slasher tx 1\n"),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = reveal:origin_tx(OriginTxs, A),
    WL = sign_tx:winners_length(OriginTx),
    SH = sign_tx:secret_hash(OriginTx),
    Number = sign_tx:number(OriginTx),
    Secret = block_tree:secret(Number, SH, ParentKey, Secrets),
    io:fwrite("slasher tx 20\n"),
    Secret = true,

    N = sign_tx:number(ST),
    io:fwrite("slasher tx 21\n"),
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    io:fwrite("slasher tx 22\n"),
    Power = block_tree:power(sign:data(block_tree:block(ParentKey))),
    io:fwrite("slasher tx 3\n"),
    DReward = fractions:multiply_int(constants:delegation_fee(), Power) div constants:validators_elected_per_block(),%half gets deleted, half is a reward.
    TReward = fractions:multiply_int(constants:slasher_reward(), ((Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL)) div 2,%???
    Acc = block_tree:account(Tx#slasher_tx.acc, ParentKey, Accounts),
    io:fwrite("slasher tx 4\n"),
    NN = accounts:update(Acc, NewHeight, TReward, 0, 1, TotalCoins),
    Nonce = accounts:nonce(NN),
    Nonce = Tx#slasher_tx.nonce,
    NewAccounts = dict:store(Tx#slasher_tx.acc, NN, Accounts),
    NewSecrets = dict:store({N, SH}, false, Secrets),
    io:fwrite("slasher tx 5\n"),
    {Channels, NewAccounts, TotalCoins - TReward, NewSecrets}.

