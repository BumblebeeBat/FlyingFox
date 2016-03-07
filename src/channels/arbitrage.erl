%If you bet on the same thing twice, it is important to keep a connection between them. So if you learn a new way of closing one of the bets, you can use the knowledge on every other time you bet the same way.
%Don't remove state from arbitrage until the highest nonced channel state we recieved from our partner doesn't have the bet.
-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, del/3,add/4,check_hash/1,second_lock/2,first_unlock/2,second_unlock/1,absorb/4,del/3,test/0]).
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
handle_cast({add, BH, ChIdLose, ChIdGain, Amount}, X) -> 
    A = {ChIdLose, ChIdGain, Amount},
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
absorb(Tx, ChIdLose, ChIdGain, Amount) ->
    io:fwrite(packer:pack(Tx)),
    1=2.
add(Bet, ChIdLose, ChIdGain, Amount) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {add, BH, ChIdLose, ChIdGain, Amount}).
del(Bet, ChIdLose, ChIdGain) -> 
    BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {del, BH, ChIdLose, ChIdGain}).
delete(Tx) ->
    io:fwrite(Tx),
    1=2.
second_lock(Tx, Amount) ->
    %Make sure that money is being sent to us on the other side of the bet first. Look in channel_manager to see if they gave it.
    %Make sure it is the same amount as before.
    io:fwrite(Tx),
    1=2.
first_unlock(Tx, Amount) ->
    %Make sure amount matches arbitrage
    %make sure amount is how much was locked on both sides.
    %Make sure we are unlocking the right one first.
    io:fwrite(Tx),
    1=2.
second_unlock(Tx) ->
    %Make sure we already did the first_unlock. Look in channel_manager to see if they gave it.
    %Make sure that the amount being unlocked this time is the same as before.
    io:fwrite(Tx),
    1=2.
check(Bet, ChId, Amount) -> 
    BH = hash:doit(Bet),
    L = check_hash(BH),
    check2(ChId, Amount, L).
check2(ChId, _, []) -> 1=2;
check2(ChId, Amount, [{ChId, ChIdGain, Amount}|T]) -> 
    
    ok;
check2(ChId, Amount, [{_, _, _}|T]) -> 
    check2(ChId, Amount, T).
check_hash(BH) -> 
    gen_server:call(?MODULE, {check, BH}).
test() ->
    add(5, 1, 0, 0),
    add(5, 2, 0, 0),
    [{2, 0, 0}, {1, 0, 0}] = check_hash(hash:doit(5)),
    X = remove({1, 2, 0}, [{2, 3, 0}, {1, 2, 0}, {1, 2, 0}]),
    X = [{2, 3, 0}, {1, 2, 0}],
    success.
