-module(sign_tx).
-export([test/0, doit/5, htoi/1, itoh/1, winner/5, sign/0, winners/1]).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
-record(sign_tx, {acc = 0, nonce = 0, secret_hash = [], winners = [], prev_hash = ""}).
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = [], power = 1, entropy = 0}).

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
-record(signed, {data="", sig="", sig2="", revealed=[]}).
sign() ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    ParentKey = block_tree:read(top),
    %ParentX = block_tree:read(ParentKey),
    PBlock = block_tree:block(ParentKey),
    %PBlock = ParentX#x.block#signed.data,
    Entropy = PBlock#block.entropy,
    FinalityAcc = accounts:read_account(Id),
    MyPower = min(Acc#acc.delegated, FinalityAcc#acc.delegated),
    TotalPower = PBlock#block.power,
    W = winners(MyPower, TotalPower, Entropy, Acc#acc.pub),
    if 
        length(W) > 0 ->
            Tx = #sign_tx{acc = Id, nonce = Acc#acc.nonce + 1, secret_hash = secrets:new(), winners = W, prev_hash = ParentKey},
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
doit(Tx, ParentKey, Channels, Accounts, Winners) ->%signers is the number of signers for this block.
    true = length(Tx#sign_tx.winners) > 0,
    Acc = block_tree:account(Tx#sign_tx.acc, ParentKey, Accounts),
    FinalityAcc = accounts:read_account(Tx#sign_tx.acc),
    MyPower = min(Acc#acc.delegated, FinalityAcc#acc.delegated),
    Block = block_tree:block(),%read(ParentKey)#signed.data,
    all_winners(MyPower, Block#block.power, Block#block.entropy, Acc#acc.pub, Tx#sign_tx.winners),
    Bond = Block#block.bond_size,
    ParentKey = Tx#sign_tx.prev_hash,
    %make sure each validator only signs the block once.
    V = max(Winners, constants:minimum_validators_per_block()),
    N = #acc{nonce = Acc#acc.nonce + 1, 
             pub = Acc#acc.pub, 
             balance = Acc#acc.balance - (Bond div V),
             delegated = Acc#acc.delegated},
    Nonce = N#acc.nonce,
    Nonce = Tx#sign_tx.nonce,%err
    true = N#acc.balance > 0,
    NewAccounts = dict:store(Tx#sign_tx.acc, N, Accounts),
    {Channels, NewAccounts}.
htoi(H) -> << I:256 >> = H, I.
itoh(I) -> << I:256 >>.
winners(L) -> winners(L, 0).
winners([], A) -> A;
winners([#signed{data = Tx}|T], A) when is_record(Tx, sign_tx)-> 
    B = A + length(Tx#sign_tx.winners),
    winners(T, B);
winners([_|T], A) -> winners(T, A).

test() ->
    H = hash:doit(1),
    H = itoh(htoi(H)),
    success.
