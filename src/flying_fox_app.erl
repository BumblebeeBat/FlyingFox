-module(flying_fox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ssl:start(),
    application:start(inets),
    D_internal = [
	 {'_', [
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
    {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}},{port, 3010}], K),
    {ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, 3011}], K_internal),

    flying_fox_sup:start_link().

%start() ->
%    application:ensure_all_started(flying_fox).

stop(_State) ->
    ok.

    
