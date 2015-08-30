-module(block_digests).%store by blockhash. so each block points to it's parent.
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/2,get/1,keys/0,test/0]).
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
handle_cast({write, K, V}, D) -> {noreply, dict:store(K, V, D)}.
write(K,V) -> gen_server:cast(?MODULE, {write, K, V}).
get(K) -> gen_server:call(?MODULE, {get, K}).
keys() -> gen_server:call(?MODULE, keys).
%example digest: {Accounts_dict, Channels_dict}
test() ->
    write(1, dict:new()),
    write(3, dict:new()),
    write(2, dict:new()),
    keys().
    
