-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/2,recieve_msg/1]).
-record(msg, {from = "", sig = "", msg = ""}).
-record(emsg, {key = "", msg = ""}).
si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
sym_enc(Key, Msg) -> bin_enc(Key, packer:pack(Msg)).
sym_dec(Key, Emsg) -> packer:unpack(bin_dec(Key, Emsg)).
send_msg(M, ToPub) -> 
    {EphPub, EphPriv} = sign:new_key(),
    Msg = #msg{from=keys:pubkey(), sig=keys:raw_sign(EphPub), msg=M},
    SS = sign:shared_secret(ToPub, EphPriv),
    Emsg = sym_enc(SS, Msg),
    #emsg{key=EphPub, msg=base64:encode(Emsg)}.
recieve_msg(Msg) ->
    Sig = sym_dec(keys:shared_secret(Msg#emsg.key), base64:decode(Msg#emsg.msg)),
    true = sign:verify_sig(Msg#emsg.key, Sig#msg.sig, Sig#msg.from),
    Sig.
test() ->
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    Sig = recieve_msg(send_msg(Record, keys:pubkey())),
    true = Sig#msg.msg == Record,
    "success".
