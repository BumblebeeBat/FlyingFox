-module(fork_slash_tx).
-export([doit/7, slasher/3]).
-record(fork_slash_tx, {acc = 0, nonce = 0, sign_tx = 0, number = 0}).%number is a block that the transgressor signed over on the main chain. That is the signature who's deposit will be slashed.
slasher(Acc, SignTx, N) -> 
    A = block_tree:account(Acc),
    io:fwrite("N is "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    #fork_slash_tx{acc = Acc, nonce = accounts:nonce(A) + 1, sign_tx = SignTx, number = N}.
    
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, Secrets, NewHeight) ->
    SignedST = Tx#fork_slash_tx.sign_tx,
    true = sign:verify(SignedST, Accounts),
    ST = sign:data(SignedST),
    A = sign_tx:acc(ST),
    PHeight = sign_tx:number(ST),
    H = PHeight + 1,
    HF = PHeight - constants:finality(),
    case block_tree:read_int(max(0, HF)) of
	error -> ok;
	Ablock ->
	    AHash = hash:doit(sign:data(Ablock)),
	    ABC = sign_tx:finality_ancestor(ST),
	    false = AHash == ABC
    end,
    true = H > -1,
    Hgap = NewHeight - H,%This is the block on a different chain they signed on.
    true = Hgap < constants:max_reveal(),
    true = Hgap > -constants:max_reveal(),
    Foo = Tx#fork_slash_tx.number, %This is the block on the blockchain that they signed over.
    Fgap = NewHeight - Foo,
    true = Fgap < constants:max_reveal(),
    HFgap = Hgap - Fgap,
    %true = HFgap > -1,
    true = HFgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(Foo, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = reveal:origin_tx(OriginTxs, A),
    WL = sign_tx:winners_length(OriginTx),
    SH = sign_tx:secret_hash(OriginTx),
    Number = sign_tx:number(OriginTx),
    Secret = block_tree:secret(Number, SH, ParentKey, Secrets),
    Secret = true,
    N = sign_tx:number(ST),
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    Power = block_tree:power(sign:data(block_tree:block(ParentKey))),
    DReward = fractions:multiply_int(constants:delegation_fee(), Power) div constants:validators_elected_per_block(),%half gets deleted, half is a reward.
    TReward = fractions:multiply_int(constants:slasher_reward(), ((Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL)) div 2,%???
    Acc = block_tree:account(Tx#fork_slash_tx.acc, ParentKey, Accounts),
    NN = accounts:update(Acc, NewHeight, TReward, 0, 1, TotalCoins),
    Nonce = accounts:nonce(NN),
    Nonce = Tx#fork_slash_tx.nonce,
    NewAccounts = dict:store(Tx#fork_slash_tx.acc, NN, Accounts),
    NewSecrets = dict:store({N, SH}, false, Secrets),

    {Channels, NewAccounts, TotalCoins - TReward, NewSecrets}.

