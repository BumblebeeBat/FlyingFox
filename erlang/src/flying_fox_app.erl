-module(flying_fox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
                                                            
    %ssl:start(),
    %application:start(inets),
    %application:ensure_all_started(flying_fox).
    flying_fox_sup:start_link().

%start() ->
%    io:fwrite("here"),
%    application:ensure_all_started(flying_fox).

stop(_State) ->
    ok.

    
