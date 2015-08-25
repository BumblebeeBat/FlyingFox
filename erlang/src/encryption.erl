-module(encryption).
-export([test/0,sym_enc/2,sym_dec/2,send_msg/2,recieve_msg/1]).
-record(msg, {from = "", sig = "", msg = ""}).
-record(emsg, {key = "", msg = ""}).
si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
sym_enc(Key, Msg) ->
    {_, X} = crypto:stream_encrypt(si(Key), packer:pack(Msg)),
    base64:encode(X).
sym_dec(Key, Emsg) ->
    X = base64:decode(Emsg),
    {_, Y} = crypto:stream_decrypt(si(Key), X),
    packer:unpack(Y).
send_msg(M, ToPub) -> 
    {EphPub, EphPriv} = sign:new_key(),
    Msg = #msg{from=keys:pubkey(), sig=keys:raw_sign(EphPub), msg=M},
    #emsg{key=EphPub, msg=sym_enc(sign:shared_secret(ToPub, EphPriv), Msg)}.
recieve_msg(Msg) ->
    Sig = sym_dec(keys:shared_secret(Msg#emsg.key), Msg#emsg.msg),
    true = sign:verify_sig(Msg#emsg.key, Sig#msg.sig, Sig#msg.from),
    Sig.
test() ->
    List = [1,2,3],
    Sig = recieve_msg(send_msg(List, keys:pubkey())),
    true = Sig#msg.msg == List,
    true = sym_dec("abc", sym_enc("abc", "1234")) == "1234",
    "success".
