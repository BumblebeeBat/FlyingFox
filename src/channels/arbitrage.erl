%If you bet on the same thing twice, it is important to keep a connection between them. So if you learn a new way of closing one of the bets, you can use the knowledge on every other time you bet the same way.
%Don't remove state from arbitrage until the highest nonced channel state we recieved from our partner doesn't have the bet.
-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,add/4,check_hash/1,agree/3,new/4,test/0]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
remove(X, [X|T]) -> T;
remove(_, []) -> [];
remove(X, [A|T]) -> [A|remove(X, T)].
handle_cast({del, BH, ChIdLose, ChIdGain, Amount}, X) -> 
    Z = case dict:find(BH, X) of
	    error -> X;
	    {ok, Val} -> 
		dict:store(BH, remove({ChIdLose, ChIdGain, Amount}, Val), X)
	end,
    {noreply, Z};
handle_cast({add, BH, ChIdLose, ChIdGain, Amount}, X) -> 
    A = {ChIdLose, ChIdGain, Amount},
    Y = case dict:find(BH, X) of
	    error -> [A];
	    {ok, Val} -> 
		%if 2 of th
		[A|Val]
	end,
    Z = dict:store(BH, Y, X),
    {noreply, Z}.
handle_call({check, BH}, _From, X) -> 
    Out = case dict:find(BH, X) of
	      error -> [];
	      {ok, Val} -> Val
	  end,
    {reply, Out, X}.
new(Tx, ChIdLose, ChIdGain, Amount) ->
    %["signed",["channel_block",0,1,-500,2,[-6,["bet",-500,[-6,0,"/rmUU2AW8ecM6TSQbyIhuc/0GWW9RLzNNSFvx/5NONY=",35,17,["f",0,1],["f",1,1],["integer",2],18,["f",0,1],["f",1,2],["integer",1],19]]],24000,false,259,0,0,0],"TUVVQ0lBR0JnL0RsZTJ1L29LckM3R01KMm9Gemhrc0xSaEpkNm5TV2dTMzdwNkVaQWlFQTNmZG41Y3JYZmw4RnVXWDNINkMyeDlvZkFSQU56bzBRaVpmUDhsZkZ6a0U9",[-6],[-6]]
    CB = sign:data(Tx),
    Bet = hd(channel_block_tx:bets(CB)),
    io:fwrite("should be "), % 2
    io:fwrite(integer_to_list(Amount)),
    io:fwrite("\n"),
    io:fwrite("is "), % -1
    io:fwrite(integer_to_list(channel_block_tx:bet_amount(Bet))),
    io:fwrite("\n"),
    To = (channel_block_tx:bet_to(Bet) * 2) - 1,
    Amount = (To * channel_block_tx:bet_amount(Bet) * -2),
    Code = channel_block_tx:bet_code(Bet),
    ChId1 = channel_block_tx:acc1(CB),
    ChId2 = channel_block_tx:acc2(CB),
    IdLose = if
	Amount > 0 -> ChId1;
	Amount < 0 -> ChId2
    end,
    ChIdLose = hd(channel_manager:id(IdLose)),
    add(Code, ChIdLose, ChIdGain, Amount).
add(Bet, ChIdLose, ChIdGain, Amount) -> 
    %Make sure we can't add the same triple twice!!
    io:fwrite("Bet code "),
    io:fwrite(packer:pack(Bet)),
    io:fwrite("\n"),
    BH = hash:doit(Bet),
    L = check_hash(BH),
    true = not_in(L, ChIdLose, ChIdGain, Amount),
    gen_server:cast(?MODULE, {add, BH, ChIdLose, ChIdGain, Amount}).
not_in([], _, _, _) -> true;
not_in([{LoseId, GainId, Amount}|T], LoseId, GainId, Amount) -> false;
not_in([_|T], A, B, C) -> not_in(T, A, B, C).
del(BH, ChIdLose, ChIdGain, Amount) -> 
    %BH = hash:doit(Bet),
    gen_server:cast(?MODULE, {del, BH, ChIdLose, ChIdGain, Amount}).
delete(Tx, BH) ->
    CB = sign:data(Tx),
    Bet = bet_find(BH, channel_block_tx:bets(CB)),
    Amount = channel_block_tx:bet_amount(Bet),
    ChId1 = channel_block_tx:acc1(Tx),
    ChId2 = channel_block_tx:acc2(Tx),
    if
	Amount > 0 -> 
	    IdLose = ChId1,
	    IdGain = ChId2;
	Amount < 0 -> 
	    IdLose = ChId2,
	    IdGain = ChId1
    end,
    ChIdLose = channel_manager:id(IdLose),
    ChIdGain = channel_manager:id(IdGain),
    ChL = channel_manager:read_channel(ChIdLose),
    ChG = channel_manager:read_channel(ChIdGain),
    BetsL = channel_block_tx:bets(ChL),
    BetsG = channel_block_tx:bets(ChG),
    true = not_in(BH, BetsL),
    true = not_in(BH, BetsG),
    del(Bet, ChIdLose, ChIdGain, Amount).
not_in(H, []) -> true;
not_in(Hash, [H|T]) -> 
    A = hash:doit(channel_block_tx:bet_code(H)),
    if
	A == H -> false;
	true -> not_in(Hash, T)
    end.
bet_find(BH, []) -> 1=2;
bet_find(BH, [H|T]) -> 
    A = hash:doit(channel_block_tx:bet_code(H)),
    if
	A == BH -> H;
	true -> bet_find(BH, T)
    end.
agree(Tx, Amount, BH) ->
    %Make sure that money is being sent to us on the other side of the bet first. Look in channel_manager to see if they gave it.
    %Make sure it is the same amount as before.
    %["signed",["channel_block",0,1,-500,2,[-6,["bet",-500,[-6,0,"/rmUU2AW8ecM6TSQbyIhuc/0GWW9RLzNNSFvx/5NONY=",35,17,["f",0,1],["f",1,1],["integer",2],18,["f",0,1],["f",1,2],["integer",1],19]]],24000,false,259,0,0,0],"TUVVQ0lBR0JnL0RsZTJ1L29LckM3R01KMm9Gemhrc0xSaEpkNm5TV2dTMzdwNkVaQWlFQTNmZG41Y3JYZmw4RnVXWDNINkMyeDlvZkFSQU56bzBRaVpmUDhsZkZ6a0U9",[-6],[-6]]
    K = keys:id(),
    CB = sign:data(Tx),
    io:fwrite("bets are "),
    io:fwrite(packer:pack(channel_block_tx:bets(CB))),
    io:fwrite("\n"),
    Bet = bet_find(BH, channel_block_tx:bets(CB)),
    To = (channel_block_tx:bet_to(Bet) * 2) - 1,
    A = channel_block_tx:bet_amount(Bet) * To * -1,
    io:fwrite("Amount "),
    io:fwrite(integer_to_list(Amount)),
    io:fwrite("\n"),
    io:fwrite("A "),
    io:fwrite(integer_to_list(A)),
    io:fwrite("\n"),
    Acc1 = channel_block_tx:acc1(CB),
    Acc2 = channel_block_tx:acc2(CB),
    P = case K of
	    Acc1 -> Acc2;
	    Acc2 -> Acc1
	end,
    ChIdGain = hd(channel_manager:id(P)),
    ChIdLoser = check_loser(channel_block_tx:bet_code(Bet), ChIdGain, Amount),
    OChannel = channel_manager:read_channel(ChIdLoser),
    Bet2 = bet_find(BH, channel_block_tx:bets(OChannel)),
    To2 = (channel_block_tx:bet_to(Bet2) * 2) - 1,
    BetAmount = channel_block_tx:bet_amount(Bet2),
    A2 = (To2 * BetAmount),
    
    %A2 = channel_block_tx:amount(OChannel),
    io:fwrite("A2 "),
    io:fwrite(integer_to_list(A2)),
    io:fwrite("\n"),
    io:fwrite("Amount "),
    io:fwrite(integer_to_list(Amount)),
    io:fwrite("\n"),
    AA = abs(A2),
    AA = abs(A),
    AA = abs(Amount div 2),
    if
	A2 < 0 ->
	    K = channel_block_tx:acc2(OChannel);
	true ->
	    K = channel_block_tx:acc1(OChannel)
    end,
    ChIdGain.
check_winner(Bet, ChIdLose, Amount) -> 
    BH = hash:doit(Bet),
    L = check_hash(BH),
    check_winner2(ChIdLose, Amount, L).
check_winner2(_, _, []) -> 1=2;
check_winner2(ChId, Amount, [{ChId, ChIdGain, Amount}|T]) -> 
    ChIdGain;
check_winner2(ChId, Amount, [{_, _, _}|T]) -> 
    check_winner2(ChId, Amount, T).
check_loser(Bet, ChIdLose, Amount) -> 
    io:fwrite("Bet code "),
    io:fwrite(packer:pack(Bet)),
    io:fwrite("\n"),
    BH = hash:doit(Bet),
    L = check_hash(BH),
    io:fwrite("L is "),
    io:fwrite(packer:pack(L)),
    io:fwrite("\n"),
    io:fwrite("Loser is "),
    io:fwrite(integer_to_list(ChIdLose)),
    io:fwrite("\n"),
    check_loser2(ChIdLose, Amount, L).
check_loser2(_, _, []) -> 1=2;
check_loser2(ChId, Amount, [{ChIdLoser, ChId, Amount}|T]) -> 
    ChIdLoser;
check_loser2(ChId, Amount, [{_, _, _}|T]) -> 
    check_loser2(ChId, Amount, T).
check_hash(BH) -> 
    gen_server:call(?MODULE, {check, BH}).
test() ->
    add(5, 1, 0, 0),
    add(5, 2, 0, 0),
    [{2, 0, 0}, {1, 0, 0}] = check_hash(hash:doit(5)),
    X = remove({1, 2, 0}, [{2, 3, 0}, {1, 2, 0}, {1, 2, 0}]),
    X = [{2, 3, 0}, {1, 2, 0}],
    success.
