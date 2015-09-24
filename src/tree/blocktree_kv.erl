-module(blocktree_kv).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, put/2,get/1,keys/0]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("kv died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(keys, _From, D) -> {reply, dict:fetch_keys(D), D};
handle_call({get, V}, _From, D) -> 
    F = dict:find(V, D),
    case F of
        {ok, Value} -> {reply, Value, D};
        error -> {reply, "none", D}
    end.
handle_cast({put, K, V}, D) -> {noreply, dict:store(K, V, D)}.
put(K,V) -> gen_server:cast(?MODULE, {put, K, V}).
get(K) -> gen_server:call(?MODULE, {get, K}).
keys() -> gen_server:call(?MODULE, keys).
