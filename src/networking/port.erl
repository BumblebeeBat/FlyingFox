-module(port).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, check/0,change/1,start_server/0]).
init(ok) -> {ok, 3010}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({change, X}, _) -> 
    {noreply, X}.
handle_call(check, _From, X) -> {reply, X, X}.

check() -> gen_server:call(?MODULE, check).
change(X) -> 
    io:fwrite("changing port!!!\n"),
    io:fwrite("changing port!!!\n"),
    if
        not is_integer(X) -> change(list_to_integer(X));
        true ->
            true = X > 0,
            gen_server:cast(?MODULE, {change, X}),
            X = check()
    end.
start_server() ->
    io:fwrite("start server!!!\n"),
    
    D_internal = [
	 {'_', [
		{"/:file", main_handler, []},
		{"/", internal_handler, []}
	       ]}
	],
    D = [
	 {'_', [
		{"/", handler, []}
	       ]}
	],
    Dispatch_internal = cowboy_router:compile(D_internal),
    Dispatch = cowboy_router:compile(D),
    K_internal = [
	 {env, [{dispatch, Dispatch_internal}]}
	],
    K = [
	 {env, [{dispatch, Dispatch}]}
	],
    {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}},{port, port:check()}], K),
    {ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, port:check() + 1}], K_internal).
