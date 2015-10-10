-module(root_pid).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, store/1,fetch/0]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, Pid}, _) -> {noreply, Pid}.
handle_call(fetch, _From, X) -> {reply, X, X}.
store(Pid) -> gen_server:cast(?MODULE, {store, Pid}).
fetch() -> gen_server:call(?MODULE, fetch).
