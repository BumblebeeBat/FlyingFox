-module(keys).
-behaviour(gen_server).
-export([doit/0]).

init([]) ->
     {ok, []}.
start_link() ->
     gen_server:start_link(?MODULE,[],[]).

%terminate(normal, Cats

doit() -> 0.
     
