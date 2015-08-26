-module(flying_fox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    internal(),
    external(),
    flying_fox_sup:start_link().

%start() ->
%    io:fwrite("here"),
%    application:ensure_all_started(flying_fox).

stop(_State) ->
    ok.
internal() ->
    Func = internal_listener,
    R = "/priv/",
    IP = {127,0,0,1},
    Routes = [{R, tcp, [Func, IP]}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    P = 9080,
    Opts = [{port, P}, {ip, IP}],
    Env = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid} = cowboy:start_http(http_internal, 100, Opts, Env).
external() ->
    Func = external_listener,
    R = "/",
    IP = {0,0,0,0},
    Routes = [{R, tcp, [Func, IP]}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    P = 8080,
    Opts = [{port, P}, {ip, IP}],
    Env = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid} = cowboy:start_http(http_external, 100, Opts, Env).
