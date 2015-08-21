-module(encryption).
-export([doit/0]).

si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
sym_enc(Key, Msg) ->
    {_, X} = crypto:stream_encrypt(si(Key), packer:pack(Msg)),
    base64:encode(X).
sym_dec(Key, Emsg) ->
    X = base64:decode(Emsg),
    {_, Y} = crypto:stream_decrypt(si(Key), X),
    packer:unpack(Y).
send_msg(Key, Topub) -> 
    0.
    %{Eph_pub, Eph_priv} = sign:new_key(),
    %E = sym_enc(sign:shared_secret(Topub, Eph_priv),#message{from=keys:}
doit() ->
    E = sym_enc("abc", "1234"),
    sym_dec("abc", E).
        
