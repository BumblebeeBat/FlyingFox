-module(flying_fox_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	flying_fox_sup:start_link().

stop(_State) ->
	ok.
