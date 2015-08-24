-module(keys).
-behaviour(gen_server).
-export([doit/0, start_link/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("keys died"), ok.
location() -> "keys".
-record(f, {pub = "", priv = ""}).
init(ok) -> 
    X = db:read(location()),
    if
        X == "" -> 
            K = #f{},
            db:save(location(),K);
        true -> K = X
    end,
    {ok, K}.

handle_call(only, _From, X) -> {reply, X, X}.
handle_cast(_, X) -> {noreply, X}.
handle_info(_, X) -> {noreply, X}.

doit() -> gen_server:call(?MODULE, only).
