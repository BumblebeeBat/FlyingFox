-module(block_tree_new).
-behaviour(gen_server).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.

-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1,read/1,account/2,account/3,test/0]).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {accounts = dict:new(), channels = dict:new(), block = 0, parent = finality}).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
handle_call({read, Hash}, _From, D) -> 
    F = dict:find(V, D),
    case F of
        {ok, Value} -> {reply, Value, D};
        error -> {reply, "none", D}
    end.
read(Hash) -> gen_server:call(?MODULE, {read, Hash}).
absorb([]) -> [];
absorb([Block|T]) -> [absorb_helper(Block)|absorb(T)].
