-module(sign_tx).
-export([test/0, doit/5, htoi/1, itoh/1, winner/5]).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
-record(sign_tx, {acc = 0, nonce = 0, secret_hash = [], winners = [], prev_hash = ""}).
winner(MyBonds, TotalBonds, Seed, Pub, J) ->
%need to add total

% https://blog.ethereum.org/2014/01/15/slasher-a-punitive-proof-of-stake-algorithm/
    %115792089237316195423570985008687907853269984665640564039457584007913129639935 = htoi(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>), %256 bits of 1111111.
    Max = 115792089237316195423570985008687907853269984665640564039457584007913129639935,
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
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = [], power = 1, entropy = 0}).
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

test() ->
    H = hash:doit(1),
    H = itoh(htoi(H)),
    success.
