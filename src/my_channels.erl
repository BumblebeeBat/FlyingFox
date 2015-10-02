-module(my_channels).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/1,remove/1,all/0,test/0]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Id, Height}, X) -> 
    {noreply, [{Id, Height}|X]};%[{ID,Height},{ID,Height}...]
handle_cast({remove, Id}, X) -> {noreply, remove_int(Id, X)}.
handle_call(all, _From, X) -> {reply, X, X}.
remove_int(Id, X) -> remove_int(Id, X, []).
remove_int(_, [], Out) -> Out;
remove_int(Id, [{Id, _}|T], Out) -> T ++ Out;
remove_int(Id, [F|T], Out) -> remove_int(Id, T, [F|Out]).
add(Id) -> gen_server:cast(?MODULE, {add, Id, block_tree:height()}).
remove(Id) -> gen_server:cast(?MODULE, {remove, Id}).
all() -> gen_server:call(?MODULE, all).

test() ->
    add(4),
    add(3),
    add(2),
    remove(3),
    [{4,0},{2,0}] = all(),
    success.

