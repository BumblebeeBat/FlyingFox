-module(sign_tx).
-export([test/0, doit/8, htoi/1, itoh/1, winner/5, sign/0, winners/1, acc/1, secret_hash/1, winners_length/1, number/1, prev_hash/1, repeat/2, finality_ancestor/1]).
-record(sign_tx, {acc = 0, nonce = 0, secret_hash = [], winners = [], prev_hash = "", number = 0, finality_ancestor = ""}).
number(T) -> T#sign_tx.number.
prev_hash(T) -> T#sign_tx.prev_hash.
secret_hash(T) -> T#sign_tx.secret_hash.
finality_ancestor(T) -> T#sign_tx.finality_ancestor.
winners_length(Tx) -> length(Tx#sign_tx.winners).
acc(Tx) -> Tx#sign_tx.acc.
winners(MyPower, TotalPower, Entropy, Pubkey) ->
    winners(MyPower, TotalPower, Entropy, Pubkey, 0, constants:chances_per_address(), []).
winners(_, _, _, _, J, Limit, Out) when J > Limit -> Out;
winners(MyPower, TotalPower, Entropy, Pubkey, J, Limit, Out) ->
    B = winner(MyPower, TotalPower, Entropy, Pubkey, J),
    if
        B -> NOut = [J|Out];
        true -> NOut = Out
    end,
    winners(MyPower, TotalPower, Entropy, Pubkey, J+1, Limit, NOut).
sign() ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ParentKey = block_tree:read(top),
    PBlock = sign:data(block_tree:block(ParentKey)),
    Entropy = block_tree:block_entropy(PBlock),
    FinalityAcc = accounts:read_account(Id),
    MyPower = min(accounts:delegated(Acc), accounts:delegated(FinalityAcc)),
    TotalPower = block_tree:block_power(PBlock),
    W = winners(MyPower, TotalPower, Entropy, accounts:pub(Acc)),
    R = repeat(Id, tx_pool:txs()),
    Height = block_tree:block_number(PBlock),
    HF = Height - constants:finality(),
    ABlock = sign:data(block_tree:read_int(max(0, HF))),
    AHash = hash:doit(ABlock),
    if 
	R -> 0;
        length(W) > 0 ->
	    Tx = #sign_tx{acc = Id, nonce = accounts:nonce(Acc) + 1, secret_hash = secrets:new(), winners = W, prev_hash = ParentKey, number = block_tree:block_number(PBlock), finality_ancestor = AHash},
            tx_pool_feeder:absorb(keys:sign(Tx));
        true ->
            io:fwrite("cannot sign, did not win this round\n")
    end.

winner(MyBonds, TotalBonds, Seed, Pub, J) ->
% https://blog.ethereum.org/2014/01/15/slasher-a-punitive-proof-of-stake-algorithm/
    Max = 115792089237316195423570985008687907853269984665640564039457584007913129639935,%= htoi(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>), %256 bits of 1111111.
    B = Max * constants:validators_elected_per_block() * MyBonds div (constants:chances_per_address() * TotalBonds),
    A = htoi(hash:doit([Seed, Pub, J])),
    (A < B) 
        and (J > -1) 
        and (J < constants:chances_per_address() + 1)
        and is_integer(J).
all_winners(_,_,_,_, []) -> 1=1;
all_winners(MyBonds, TotalBonds, Seed, Pub, [H|T]) ->
    true = winner(MyBonds, TotalBonds, Seed, Pub, H),
    all_winners(MyBonds, TotalBonds, Seed, Pub, T).
doit(Tx, Txs, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight) ->%signers is the number of signers for this block.
    WL = length(Tx#sign_tx.winners),
    true = WL > 0,
    Acc = block_tree:account(Tx#sign_tx.acc, ParentKey, Accounts),
    FinalityAcc = accounts:read_account(Tx#sign_tx.acc),
    MyPower = min(accounts:delegated(Acc), accounts:delegated(FinalityAcc)),
    Block = sign:data(block_tree:block(block_tree:read(ParentKey))),
    Pnum = block_tree:block_number(Block),
    Pnum = Tx#sign_tx.number,
    all_winners(MyPower, block_tree:block_power(Block), block_tree:block_entropy(Block), accounts:pub(Acc), Tx#sign_tx.winners),
    ParentKey = Tx#sign_tx.prev_hash,
    false = repeat(Tx#sign_tx.acc, Txs),%makes sure each validator only signs the block once.
    Lose = fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)* WL,
    N = accounts:update(Acc, NewHeight, -Lose, 0, 1, TotalCoins),
    Nonce = accounts:nonce(N),
    Nonce = Tx#sign_tx.nonce,
    NewAccounts = dict:store(Tx#sign_tx.acc, N, Accounts),
    SH = Tx#sign_tx.secret_hash,
    %SHstate = block_tree:secret(NewHeight, SH, ParentKey, SecretHashes),
    %SHstate = false,
    NewSecretHash = dict:store({Pnum, SH}, true, SecretHashes),%newheight should instead be the height of the previous block.
    PBlock = sign:data(block_tree:block(ParentKey)),
    Height = block_tree:block_number(PBlock),
    HF = Height - constants:finality(),
    ABlock = sign:data(block_tree:read_int(max(0, HF))),
    AHash = hash:doit(ABlock),
    AHash = Tx#sign_tx.finality_ancestor,
    {Channels, NewAccounts, TotalCoins - Lose, NewSecretHash}.
repeat(_, []) -> false;
repeat(Accn, [SignedTx|Txs]) ->
    Tx = sign:data(SignedTx),
    if
	is_record(Tx, sign_tx) and (Tx#sign_tx.acc == Accn) -> true;
	true -> repeat(Accn, Txs)
    end.
	    
htoi(H) -> << I:256 >> = H, I.
itoh(I) -> << I:256 >>.
winners(L) -> winners(L, 0).
winners([], A) -> A;
winners([SignedTx|T], A) ->
    Tx = sign:data(SignedTx),
    if
	is_record(Tx, sign_tx) ->
	    B = A + length(Tx#sign_tx.winners),
	    winners(T, B);
	true -> winners(T, A)
    end.

test() ->
    H = hash:doit(1),
    H = itoh(htoi(H)),
    success.
