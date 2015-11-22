-module(handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, D, Req),
    {ok, Req2, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
-define(WORD, 10000000).%10 megabytes.
doit({pubkey}) -> {ok, keys:pubkey()};
doit({height}) -> {ok, block_tree:height()};
doit({block, N}) -> {ok, block_tree:block(block_tree:read_int(N))};
doit({tophash}) -> {ok, hash:doit(block_tree:top())};
doit({recent_hash, H}) -> {ok, block_tree:is_key(H)};
doit({accounts_size}) ->
    {ok, filelib:file_size("backup/accounts.db") div ?WORD};
doit({tx_absorb, Tx}) -> 
    {ok, tx_pool:absorb(Tx)};
doit({accounts, N}) ->
    {ok, File} = file:open("backup/accounts.db", [read, binary, raw]),
    O = case file:pread(File, ?WORD * N, ?WORD) of
	    eof -> "eof";
	    {ok, Out} -> Out;
	    {error, Reason} -> 
		io:fwrite("file read error\n"),
		Reason
	end,
    file:close(File),
    {ok, O};
doit({channel_recieve, ChId, MinAmount, Ch}) ->
    {ok, channel_manager:recieve(ChId, MinAmount, Ch)};
doit({locked_payment, From, To, Payment, Amount, SecretHash}) ->
    ChIdFrom = hd(channel_manager:id(From)),
    Return = channel_manager:recieve_locked_payment(ChIdFrom, Payment, Amount, SecretHash),
    ChIdTo = hd(channel_manager:id(To)),
    Payment2 = channel_manager:hashlock(ChIdTo, Amount, SecretHash),
    mail:internal_send(To, Payment2, free_constants:hashlock_time()),%undefined.
    {ok, Return};
doit({txs}) -> {ok, tx_pool:txs()};
doit({unlock, ChId, Secret, SignedCh}) ->
    {ok, channel_manager:unlock_hash(ChId, Secret, SignedCh)};
doit({register, Payment, Acc}) ->
    {ok, mail:register(Payment, Acc)};
doit({send, Payment, To, Msg, Seconds}) ->
    mail:send(Payment, To, Msg, Seconds),
    {ok, 0};
doit({id}) -> {ok, keys:id()};
doit({pop, Msg}) ->
    {ok, mail:pop(Msg)};
doit({register_cost}) ->
    {ok, mail:register_cost()};
%need a way to share recent txs.			   
%I want to share the backup version of all the files.
doit({nonce, ID}) ->
    A = nonce:customer_get(ID),
    {ok, A};
doit({new_channel, SignedTx}) ->
    %make sure the Tx is more than 1/2 funded by the other person.
    %make sure the amount of money is below some limit.
    Tx = sign:data(SignedTx),
    true = to_channel_tx:half_them(Tx),
    MC = free_constants:max_channel(), 
    MS = to_channel_tx:my_side(Tx),
    true = MC > MS,
    NTx = keys:sign(SignedTx),
    tx_pool:absorb(NTx),
    {ok, NTx};
doit({channel_spend, Payment, Partner}) ->
    ChId = hd(channel_manager:id(Partner)),
    channel_manager:recieve(ChId, 0, Payment),
    Tx = sign:data(Payment),
    R = keys:sign(Tx),
    {ok, R};
doit(X) ->
    io:fwrite("I can't handle this \n"),
    io:fwrite(packer:pack(X)), %unlock2
    {error}.
    
