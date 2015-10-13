-module(sign_tx).
-export([test/0, doit/5, htoi/1, itoh/1, winner/5, sign/0, winners/1, acc/1]).
-record(sign_tx, {acc = 0, nonce = 0, secret_hash = [], winners = [], prev_hash = ""}).
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
    PBlock = block_tree:block(ParentKey),
    Entropy = block_tree:block_entropy(PBlock),
    FinalityAcc = accounts:read_account(Id),
    MyPower = min(accounts:delegated(Acc), accounts:delegated(FinalityAcc)),
    TotalPower = block_tree:block_power(PBlock),
    W = winners(MyPower, TotalPower, Entropy, accounts:pub(Acc)),
    if 
        length(W) > 0 ->
            Tx = #sign_tx{acc = Id, nonce = accounts:nonce(Acc) + 1, secret_hash = secrets:new(), winners = W, prev_hash = ParentKey},
            tx_pool:absorb(keys:sign(Tx));
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
doit(Tx, ParentKey, Channels, Accounts, NewHeight) ->%signers is the number of signers for this block.
    WL = length(Tx#sign_tx.winners),
    true = WL > 0,
    Acc = block_tree:account(Tx#sign_tx.acc, ParentKey, Accounts),
    FinalityAcc = accounts:read_account(Tx#sign_tx.acc),
    MyPower = min(accounts:delegated(Acc), accounts:delegated(FinalityAcc)),
    Block = block_tree:block(),
    all_winners(MyPower, block_tree:block_power(Block), block_tree:block_entropy(Block), accounts:pub(Acc), Tx#sign_tx.winners),
    ParentKey = Tx#sign_tx.prev_hash,
    %make sure each validator only signs the block once.
    N = accounts:update(Acc, NewHeight, (-(constants:security_bonds_per_winner() * WL)), 0, 1),
    Nonce = accounts:nonce(N),
    Nonce = Tx#sign_tx.nonce,
    NewAccounts = dict:store(Tx#sign_tx.acc, N, Accounts),
    {Channels, NewAccounts}.
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
