-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/2,get_msg/1,msg/1,id/1]).
-record(msg, {sig = "", msg = "", id = 0}).
-record(emsg, {key = "", msg = ""}).
msg(Msg) -> Msg#msg.msg.
id(Msg) -> Msg#msg.id.

si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
sym_enc(Key, Msg) -> bin_enc(Key, term_to_binary(Msg)).
sym_dec(Key, Emsg) -> binary_to_term(bin_dec(Key, Emsg)).
send_msg(M, ToPub) -> 
    {EphPub, EphPriv} = sign:new_key(),
    Msg = #msg{sig=keys:raw_sign(EphPub), msg=M, id = keys:id()},
    SS = sign:shared_secret(ToPub, EphPriv),
    Emsg = sym_enc(SS, Msg),
    #emsg{key=EphPub, msg=base64:encode(Emsg)}.
get_msg(Msg) ->
    Sig = sym_dec(keys:shared_secret(Msg#emsg.key), base64:decode(Msg#emsg.msg)),
    Acc = block_tree:account(Sig#msg.id),
    true = sign:verify_sig(Msg#emsg.key, Sig#msg.sig, accounts:pub(Acc)),
    Sig.
test() ->
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    Sig = get_msg(send_msg(Record, keys:pubkey())),
    true = Sig#msg.msg == Record,
    success.
