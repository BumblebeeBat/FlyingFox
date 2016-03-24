-module(temp_rel_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	temp_rel_sup:start_link().

stop(_State) ->
	ok.
