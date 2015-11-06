-module(handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    io:fwrite("handle data "),
    io:fwrite(Data),
    io:fwrite("\n"),
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
    io:fwrite("absorb a tx"),
    io:fwrite(packer:pack(Tx)),
    io:fwrite("\n"),
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
doit({channel_locked_payment, ChId, Ch}) ->
    {ok, channel_manager:recieve_locked_payment(ChId, Ch)};
doit({txs}) -> {ok, tx_pool:txs()};
doit({unlock2, ChId, Secret, SignedCh}) ->
    {ok, channel_manager:unlock_hash(ChId, Secret, SignedCh)};
%need a way to share recent txs.			   
%I want to share the backup version of all the files.
doit(X) ->
    io:fwrite("I can't handle this \n"),
    io:fwrite(packer:pack(X)), %unlock2
    {error}.
    
