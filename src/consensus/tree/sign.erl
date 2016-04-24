-module(sign).
-export([test/0,new_key/0,sign_tx/4,sign_tx/5,sign/2,verify_sig/3,shared_secret/2,verify/2,data/1,revealed/1,empty/1,empty/0,set_revealed/2,verify_1/2,verify_2/2]).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
empty() -> #signed{}.
empty(X) -> #signed{data=X}.
data(X) -> X#signed.data.
set_revealed(X, R) -> #signed{data = X#signed.data, sig = X#signed.sig, sig2 = X#signed.sig2, revealed = R}.
revealed(X) -> X#signed.revealed.
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
new_key() -> 
    {Pub, Priv} = crypto:generate_key(ecdh, params()),
    {en(Pub), en(Priv)}.
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, term_to_binary(S), [de(Priv), params()])).
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = de(Pub),
    crypto:verify(ecdsa, sha256, term_to_binary(S), SD, [PD, params()]).
verify_1(Tx, Pub) -> verify_sig(Tx#signed.data, Tx#signed.sig, Pub).
verify_2(Tx, Pub) -> verify_sig(Tx#signed.data, Tx#signed.sig2, Pub).
verify_both(Tx, Pub1, Pub2) ->
    X = verify_1(Tx, Pub1),
    Y = verify_2(Tx, Pub1),
    if
        X -> verify_2(Tx, Pub2);
        Y -> verify_1(Tx, Pub2);
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
	    verify_both(SignedTx, accounts:pub(Acc1), accounts:pub(Acc2));
	true -> verify_1(SignedTx, accounts:pub(Acc1))
    end.
sign_tx(SignedTx, Pub, Priv, Accounts) ->
    sign_tx(SignedTx, Pub, Priv, keys:id(), Accounts).
sign_tx(SignedTx, Pub, Priv, ID, Accounts) when element(1, SignedTx) == signed ->
    Tx = SignedTx#signed.data,
    R = SignedTx#signed.revealed,
    N = element(2, Tx),
    Acc = block_tree:account(N, Accounts),
    APub = accounts:pub(Acc),
    if
	(APub == Pub) and (N == ID) -> 
	    Sig = sign(Tx, Priv),
	    #signed{data=Tx, sig=Sig, sig2=SignedTx#signed.sig2, revealed=R};
	true ->
	    N2 = element(3, Tx),
	    Acc2 = block_tree:account(N2, Accounts),
	    BPub = accounts:pub(Acc2),
	    if
		(Pub == BPub) and (N2 == ID)->
		    Sig = sign(Tx, Priv),
		    #signed{data=Tx, sig=SignedTx#signed.sig, sig2=Sig, revealed=R};
		true -> {error, <<"cannot sign">>}
	    end
    end;
sign_tx(Tx, Pub, Priv, ID, Accounts) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    Acc = block_tree:account(N, Accounts),
    APub = accounts:pub(Acc),
    if
	APub == Pub -> #signed{data=Tx, sig=Sig};
	true ->
	    N2 = element(3, Tx),
	    Acc2 = block_tree:account(N2, Accounts),
	    Pub = accounts:pub(Acc2),
	    #signed{data=Tx, sig2=Sig}
    end.

test() ->
    {Pub, Priv} = new_key(),
    {Pub2, Priv2} = new_key(),
    Acc = accounts:empty(Pub),
    Acc2 = accounts:empty(Pub2),
    Accounts = dict:store(1, Acc2, dict:store(0, Acc, dict:new())),
    Tx = {channel_block, 0, 1},
    Signed = sign_tx(sign_tx(Tx, Pub, Priv, Accounts), Pub2, Priv2, Accounts),
    Signed2 = sign_tx({spend, 0}, Pub, Priv, Accounts),
    true = verify(Signed2, Accounts),
    true = verify(Signed, Accounts),
    true = verify_both(Signed, Pub2, Pub) 
        and (verify_both(Signed, Pub, Pub2)
        and not verify_both(Signed, Pub, Pub)),
    success.
