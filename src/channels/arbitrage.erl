%If you bet on the same thing twice, it is important to keep a connection between them. So if you learn a new way of closing one of the bets, you can use the knowledge on every other time you bet the same way.
%Don't remove state from arbitrage until the highest nonced channel state we recieved from our partner doesn't have the bet.
-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, del/3,add/3,check/1,check_hash/1,test/0]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
remove(X, [X|T]) -> T;
remove(_, []) -> [];
remove(X, [A|T]) -> [A|remove(X, T)].
handle_cast({del, BH, ChIdLose, ChIdGain}, X) -> 
    Z = case dict:find(BH, X) of
	    error -> X;
	    {ok, Val} -> 
		dict:store(BH, remove({ChIdLose, ChIdGain}, Val), X)
	end,
    {noreply, Z};
handle_cast({add, BH, ChIdLose, ChIdGain}, X) -> 
    A = {ChIdLose, ChIdGain},
    Y = case dict:find(BH, X) of
	    error -> [A];
	    {ok, Val} -> [A|Val]
	end,
    Z = dict:store(BH, Y, X),
    {noreply, Z}.
handle_call({check, BH}, _From, X) -> 
    Out = case dict:find(BH, X) of
	      error -> [];
	      {ok, Val} -> Val
	  end,
    {reply, Out, X}.
absorb(Tx, ChIdLose, ChIdGain) ->
    io:fwrite(packer:pack(Tx)),
    1=2.
add(Bet, ChIdLose, ChIdGain) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {add, BH, ChIdLose, ChIdGain}).
del(Bet, ChIdLose, ChIdGain) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {del, BH, ChIdLose, ChIdGain}).
check(Bet) -> 
    BH = hash:doit(Bet),
    check_hash(BH).
check_hash(BH) -> 
    gen_server:call(?MODULE, {check, BH}).
test() ->
    add(5, 1, 0),
    add(5, 2, 0),
    [{2, 0}, {1, 0}] = check(5),
    X = remove({1, 2}, [{2, 3}, {1, 2}, {1, 2}]),
    X = [{2, 3}, {1, 2}],
    success.
