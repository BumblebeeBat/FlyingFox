%If you bet on the same thing twice, it is important to keep a connection between them. So if you learn a new way of closing one of the bets, you can use the knowledge on every other time you bet the same way
-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, del/2,add/2,check/1,check_hash/1,test/0]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
remove(X, [X|T]) -> T;
remove(_, []) -> [];
remove(X, [A|T]) -> [A|remove(X, T)].
handle_cast({del, BH, ChId}, X) -> 
    Z = case dict:find(BH, X) of
	    error -> X;
	    {ok, Val} -> 
		dict:store(BH, remove(ChId, Val), X)
	end,
    {noreply, Z};
handle_cast({add, BH, ChId}, X) -> 
    Y = case dict:find(BH, X) of
	    error -> [ChId];
	    {ok, Val} -> [ChId|Val]
	end,
    Z = dict:store(BH, Y, X),
    {noreply, Z}.
handle_call({check, BH}, _From, X) -> 
    Out = case dict:find(BH, X) of
	      error -> [];
	      {ok, Val} -> Val
	  end,
    {reply, Out, X}.
add(Bet, ChId) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {add, BH, ChId}).
del(Bet, ChId) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {del, BH, ChId}).
check(Bet) -> 
    BH = hash:doit(Bet),
    check_hash(BH).
check_hash(BH) -> 
    gen_server:call(?MODULE, {check, BH}).
test() ->
    add(5, 1),
    add(5, 2),
    [2, 1] = check(5),
    success.
