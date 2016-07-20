-module(sign).
-export([test/0,test2/1,test3/0,sign_tx/5,sign/2,verify_sig/3,shared_secret/2,verify/2,data/1,revealed/1,empty/1,empty/0,set_revealed/2,verify_1/2,verify_2/2, pubkey2address/1, valid_address/1, hard_new_key/0,new_key/0,pub/1,pub2/1]).
-record(signed, {data="", sig="", pub = "", sig2="", pub2="", revealed=[]}).
pub(X) -> X#signed.pub.
pub2(X) -> X#signed.pub2.
empty() -> #signed{}.
empty(X) -> #signed{data=X}.
data(X) -> X#signed.data.
set_revealed(X, R) -> #signed{data = X#signed.data, sig = X#signed.sig, pub = X#signed.pub, sig2 = X#signed.sig2, pub2 = X#signed.pub2, revealed = R}.
revealed(X) -> X#signed.revealed.
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
%to_bytes(X) -> term_to_binary(X).
to_bytes(X) -> packer:pack(X).
new_key() -> %We keep this around for the encryption library. it is used to generate 1-time encryption keys.
    {Pub, Priv} = crypto:generate_key(ecdh, params()),
    {en(Pub), en(Priv)}.
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, to_bytes(S), [de(Priv), params()])).
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = de(Pub),
    crypto:verify(ecdsa, sha256, to_bytes(S), SD, [PD, params()]).
verify_1(Tx, Addr) -> 
    Pub = Tx#signed.pub,
    B = verify_sig(Tx#signed.data, Tx#signed.sig, Pub),
    (Addr == pubkey2address(Pub)) and B.
verify_2(Tx, Addr) -> 
    Pub2 = Tx#signed.pub2,
    B = verify_sig(Tx#signed.data, Tx#signed.sig2, Pub2),
    (Addr == pubkey2address(Pub2)) and B.
verify_both(Tx, Addr1, Addr2) ->
    X = verify_1(Tx, Addr1),
    Y = verify_2(Tx, Addr1),
    if
        X -> verify_2(Tx, Addr2);
        Y -> verify_1(Tx, Addr2);
        true -> false
    end.
verify(SignedTx, Accounts) ->
    Tx = SignedTx#signed.data,
    N1 = element(2, Tx),
    Acc1 = block_tree:account(N1, Accounts),
    Type = element(1, Tx),
    
    if
	(Type == channel_block) or (Type == tc) ->
	    N2 = element(3, Tx),
	    Acc2 = block_tree:account(N2, Accounts),
	    verify_both(SignedTx, accounts:addr(Acc1), accounts:addr(Acc2));
	true -> verify_1(SignedTx, accounts:addr(Acc1))
    end.
sign_tx(SignedTx, Pub, Priv, ID, Accounts) when element(1, SignedTx) == signed ->
    Tx = SignedTx#signed.data,
    R = SignedTx#signed.revealed,
    N = element(2, Tx),
    Acc = block_tree:account(N, Accounts),
    AAddr = accounts:addr(Acc),
    %APub = accounts:pub(Acc),
    Addr = pubkey2address(Pub),
    if
	(AAddr == Addr) and (N == ID) -> 
	    Addr = accounts:addr(Acc),
	    Sig = sign(Tx, Priv),
	    #signed{data=Tx, sig=Sig, pub=Pub, sig2=SignedTx#signed.sig2, pub2=SignedTx#signed.pub2, revealed=R};
	true ->
	    N2 = element(3, Tx),
	    Acc2 = block_tree:account(N2, Accounts),
	    BAddr = accounts:addr(Acc2),
	    %BPub = accounts:pub(Acc2),
	    if
		((Addr == BAddr) and (N2 == ID)) ->
		    Sig = sign(Tx, Priv),
		    #signed{data=Tx, sig=SignedTx#signed.sig, pub=SignedTx#signed.pub, sig2=Sig, pub2=Pub, revealed=R};
		true -> {error, <<"cannot sign">>}
	    end
    end;
sign_tx(Tx, Pub, Priv, ID, Accounts) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    Acc = block_tree:account(N, Accounts),
    AAddr = accounts:addr(Acc),
    Addr = pubkey2address(Pub),
    %APub = accounts:pub(Acc),
    N2 = element(3, Tx),
    if
	((Addr == AAddr) and (N == ID)) -> 
	    #signed{data=Tx, sig=Sig, pub=Pub};
	(N2 == ID) ->
	    Acc2 = block_tree:account(N2, Accounts),
	    Addr = accounts:addr(Acc2),
	    #signed{data=Tx, sig2=Sig, pub2=Pub};
	true -> {error, <<"cannot sign">>}
    end.

checksum(X) -> 
    checksum(0, X).
checksum(N, <<H:4, T/bitstring>>) ->
    checksum(N+H, <<T/bitstring>>);
checksum(N, <<>>) ->
    M = N rem 16,
    <<M:4>>.

%looks like 60,000,000 keys per second costs about $1 a month. https://en.bitcoin.it/wiki/Vanitygen
%there are 2,600,000 seconds per month, so they can test 1.5*10^14 addresses for $1.
%assume the attacker has $1 billion, they can test 1.5*10^23 addresses.
%That is 77 bits.
%Assuming there are 1 million accounts with enough money in them that we can profitably attack simultaniously, add 20 bits
% 97 bits.
% make address generation 100,000 times more difficult, so we lose 15 bits.
% 82 bits.
% to stay ahead of moorse law speedup, add another two bits.
% 84 bits.

%there are 30 bits of humans (a little under 10 billion).
%by birthday problem, we need at least 60 bit of addresses to stop humans from randomly finding a collision.
%Each human is willing to pay up to $0.001 to buy an address.
%Attacker is willing to pay $1 billion to attack.
%there is 36 bits difference between
%If there were 1000000 people rich enough to be simultaniously attacked, then we need 20 more bits of security.
%36+20=56
%So at the minimum, an address would need to have 60 bits.
-define(AddressEntropy, constants:address_entropy()).
pubkey2address(P) when size(P) > 66 ->
    pubkey2address(base64:decode(P));
pubkey2address(P) ->
    AB = (?AddressEntropy + 4),
    BC = 256 - AB,
    << A:AB, T:BC >> = hash:doit(P),
    S = T rem 5000,
    case S of
	0 ->
	    <<C:4>> = checksum(<<A:(?AddressEntropy)>>),
	    D = <<C:4, A:(?AddressEntropy) >>,
	    list_to_binary(base58:binary_to_base58(D));
	_ ->
	    {error, invalid_pubkey}
    end.
valid_address(A) ->
    AB = ?AddressEntropy,
    << C:4, B:AB >> = base58:base58_to_binary(binary_to_list(A)),
    D = checksum(<<B:AB>>),
    << C:4 >> == D.

test() ->
    %{Address, Pub, Priv} = hard_new_key(), %recomputing is too slow. better to write it down, and reuse it each time.
    {Address, Pub, Priv} = hard_new_key(),
    {Address2, Pub2, Priv2} = hard_new_key(),
    Acc = accounts:empty(Address),
    Acc2 = accounts:empty(Address2),
    Accounts = dict:store(1, Acc2, dict:store(0, Acc, dict:new())),
    Tx = {channel_block, 0, 1},
    Signed = sign_tx(sign_tx(Tx, Pub, Priv, 0, Accounts), Pub2, Priv2, 1, Accounts),
    Signed2 = sign_tx({spend, 0, 0, 1, 1, 1}, Pub, Priv, 0, Accounts),
    Verbose = true,
    if
	Verbose ->
	    io:fwrite("pubkeys\n"),
	    io:fwrite(Pub),
	    io:fwrite("\n"),
	    io:fwrite(Pub2),
	    io:fwrite("\n"),
	    io:fwrite("privkeys\n"),
	    io:fwrite(Priv),
	    io:fwrite("\n"),
	    io:fwrite(Priv2),
	    io:fwrite("\n"),
	    io:fwrite("signed tx\n"),
	    io:fwrite(packer:pack(Signed)),
	    io:fwrite("\n");
	true -> ok
    end,
    true = verify(Signed2, Accounts),
    true = verify(Signed, Accounts),
    true = verify_both(Signed, Address, Address2),
    true = verify_both(Signed, Address2, Address),
    false = verify_both(Signed, Address, Address),
    true = valid_address(Address),
    success.
next_priv(Priv) ->
    Bits = bit_size(Priv),
    <<Integer:Bits>> = Priv,
    N = Integer + 1,
    <<N:Bits>>.
hard_new_key() ->
    {_, Priv} = crypto:generate_key(ecdh, params()),
    hard_new_key_2(Priv).
hard_new_key_2(Priv) ->
    %io:fwrite("hard 2"),
    {Pub, Priv} = crypto:generate_key(ecdh, params(), Priv),
    Address = pubkey2address(Pub),
    case Address of
	{error, _} -> hard_new_key_2(next_priv(Priv));
	Address -> {Address, en(Pub), en(Priv)}
    end.
generate() ->
    crypto:generate_key(ecdh, params()).
times(0, _) -> ok;
times(N, F) ->
    F(),
    times(N-1, F).
test2(X) ->
    times(X, fun() -> generate() end ).
test3() ->
    %timer:tc(sign, test2, [X]).
    timer:tc(sign, hard_new_key, []).
		     

