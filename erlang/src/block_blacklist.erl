%if 2/3 of validators signed on an invalid block, which causes them to lose their deposit, then that block hash will be in this list until finality.

-module(block_blacklist).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, is_key/1,append/2,remove/1,remove_old/1,test/0]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
-define(file, "blacklist.db").
handle_cast({append, Key, Height}, L) -> {noreply, dict:store(Key, Height, L)};
handle_cast({remove, Key}, L) -> {noreply, dict:erase(Key, L)};
handle_cast({remove_old, Height}, L) -> 
    F = fun(_, Value) -> Value > Height-26 end,
    {noreply, dict:filter(F, L)}.
handle_call({key, Key}, _From, L) -> 
    Out = case dict:find(Key, L) of
	error -> false;
	{ok, _} -> true
    end,
    {reply, Out, L}.
is_key(Key) -> gen_server:call(?MODULE, {key, Key}).
append(Key, Height) -> gen_server:cast(?MODULE, {append, Key, Height}).
remove(Key) -> gen_server:cast(?MODULE, {remove, Key}).
remove_old(Height) ->gen_server:cast(?MODULE, {remove_old, Height}).
test() ->
    BH = <<1,2,3>>,
    false = is_key(BH),
    append(BH, 2),
    true = is_key(BH),
    remove(BH),
    false = is_key(BH),
    append(BH, 3),
    true = is_key(BH),
    remove_old(2+26),
    true = is_key(BH),
    remove_old(3+26),
    false = is_key(BH),
    success.
