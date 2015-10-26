-module(main_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
-define(file, "src/main.js").
handle(Req, State) ->
    {ok, _Data, _} = cowboy_req:body(Req),
    Headers = [{<<"content-type">>, <<"text/html">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Text = read_file(),
    {ok, Req2} = cowboy_req:reply(200, Headers, Text, Req),
    {ok, Req2, State}.
read_file() ->
    {ok, File } = file:open(?file, [read, binary, raw]),
    {ok, O} =file:pread(File, 0, filelib:file_size(?file)),
    file:close(File),
    O.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
