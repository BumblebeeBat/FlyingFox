-module(tcp).
-export([init/3,body/2,handler/2,terminate/3, get/4,get/3,get_local/3,test/0]).
init({tcp, http}, Req, Opts) -> {ok, Req, Opts}.
body(Req, Opts) ->
    {Status, Data, Req2} = cowboy_req:body(Req, Opts),
    case Status of
        ok -> Data;
        more -> timer:sleep(5), 
                body(Req2, Opts)
    end.
handler(Req, Opts) ->            
    [Func, _] = Opts,
		Headers = [{"content-type", "application/octet-stream"},
							 {"Access-Control-Allow-Origin", "*"}],
    A = body(Req, Opts),
    io:fwrite("handler\n"),
    B = packer:unpack(A),
    C = packer:pack(Func(B)),
    {ok, Resp} = cowboy_req:reply(200, Headers, C, Req),
    {ok, Resp, Opts}.
terminate(_Reason, _Req, _State) -> ok.
get_local(Ip, Port, X) -> get(Ip, Port, X, "/priv").
get(IP, Port, A) -> get(IP, Port, A, "").
get(Ip, Port, A, Y) ->
    Url = "http://" ++ inet_parse:ntoa(Ip) ++ ":" ++ integer_to_list(Port) ++ Y,
    Content_type = "application/octet-stream",
    Request = {Url, [], Content_type, packer:pack(A)},
    X = httpc:request(post, Request, [], []),
    case X of
        {ok, Z} -> packer:unpack(Z);
        {error, socket_closed_remotely} -> get(Ip, Port, A, Y);
        {error, {failed_connect, _}} -> "";
        X ->
            io:fwrite("TCP WEIRD"),
            X
    end.
test() -> get({127,0,0,1}, 8080, <<"hello world">>).
