-module(flying_fox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(inets),
    D = [
	 {'_', [
		{"/", flying_fox_handler, []}
	       ]}
	],
    Dispatch = cowboy_router:compile(D),
    K = [
	 {env, [{dispatch, Dispatch}]}
	],
    {ok, _} = cowboy:start_http(http, 100, [{port, 3010}], K),

    flying_fox_sup:start_link().

%start() ->
%    application:ensure_all_started(flying_fox).

stop(_State) ->
    ok.

    
