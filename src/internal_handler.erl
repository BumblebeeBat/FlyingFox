-module(internal_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    D = packer:pack(doit(packer:unpack(Data))),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/octet-stream">>}], D, Req),
    {ok, Req2, State}.

init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
doit({test}) -> {test_response};
doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(X),
    io:fwrite("\n"),
    {error}.
