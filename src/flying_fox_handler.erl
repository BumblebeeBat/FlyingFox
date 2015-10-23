-module(flying_fox_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d echo=echomeplz http://localhost:3010
handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/octet-stream">>}], Data, Req),
    {ok, Req2, State}.

init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.

