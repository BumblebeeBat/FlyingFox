%this module needs to keep track of the highest-nonced transaction recieved in each channel.

-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, unlock_hash/2,hashlock/3,spend/2,recieve/3,read/1,new_channel/1,first_cb/2,recieve_locked_payment/2,delete/1,id/1]).
-record(f, {channel = [], unlock = []}).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, N, Ch}, X) -> 
    {noreply, dict:store(N, Ch, X)};
handle_cast({delete, N}, X) -> 
    {noreply, dict:erase(N, X)}.
handle_call({read, N}, _From, X) -> 
    {reply, dict:fetch(N, X), X};
handle_call(keys, _From, X) -> 
    {reply, dict:keys(X), X}.
repeat(Times, X) -> repeat(Times, X, []).
repeat(0, _, Out) -> Out;
repeat(Times, X, Out) -> repeat(Times-1, X, [X|Out]).
keys() -> gen_server:call(?MODULE, keys).
id(Partner) -> id_helper(Partner, keys(), []).
id_helper(_, [], Out) -> Out;
id_helper(Partner, [Key|T], Out) ->
    F = read(Key),
    Ch = F#f.channel,
    Acc1 = channel_block_tx:acc1(Ch),
    Acc2 = channel_block_tx:acc2(Ch),
    NewOut = if
	((Partner == Acc1) or (Partner == Acc2)) -> [Key|Out];
	true -> Out
	     end,
    id_helper(Partner, T, NewOut).

store(ChId, F) -> 
    gen_server:cast(?MODULE, {store, ChId, F}).

new_channel(ChId) -> 
    F = #f{},
    store(ChId, F).

first_cb(ChId, CB) ->
    Old = read(ChId),
    true = is_record(f, Old),
    F = #f{channel = CB, unlock = repeat(length(channel_block_tx:bets(CB)), 28)},
    store(ChId, F).
    

read(ChId) -> gen_server:call(?MODULE, {read, ChId}).
delete(ChId) -> gen_server:call(?MODULE, {delete, ChId}).
spend(ChId, Amount) ->
    F = read(ChId),
    Ch = F#f.channel,
    NewCh = channel_block_tx:update(Ch, Amount, 1),
    NewF = #f{channel = NewCh, unlock = F#f.unlock},
    store(ChId, NewF).
match_n(X, Bets) -> match_n(X, Bets, 0).
match_n(X, [Bet|Bets], N) ->
    Y = language:extract_sh(channel_block_tx:bet_code(Bet)),
    if
        X == Y -> N;
        true -> match_n(X, Bets, N+1)
    end.
replace_n(N, New, L) -> replace_n(N, New, L, []).
replace_n(0, New, [_|L], Out) -> lists:reverse(Out) ++ [New] ++ L.
unlock_hash(ChId, Secret) ->
    SecretHash = hash:doit(Secret),
    F = read(ChId),
    Ch = F#f.channel,
    N = match_n(SecretHash, channel_block_tx:bets(Ch)),
    Unlock = F#f.unlock,
    NewUnlock = replace_n(N, [Secret], Unlock),
    %channel_block_tx:add,
    NewF = #f{channel = Ch, unlock = NewUnlock},
    store(ChId, NewF).
hashlock(ChId, Amount, SecretHash) ->
    F = read(ChId),
    Ch = F#f.channel,
    Ch2 = channel_block_tx:update(Ch, Amount div 2, 1),
    Channel = block_tree:channel(ChId),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    ToAmount = case keys:id() of
            Acc1 -> 0;
            Acc2 -> 1
        end,
    Script = language:hashlock(ToAmount, SecretHash),
    Ch3 = channel_block_tx:add_bet(Ch2, Amount div 2, Script),
    NewF = #f{channel = Ch3, unlock = [[1]|F#f.unlock]},
    store(ChId, NewF).
recieve_locked_payment(ChId, NewCh) ->
    channel_block_tx:is_cb(NewCh),
    F = read(ChId),
    Ch = F#f.channel,
    NewAmount = channel_block_tx:amount(NewCh),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(NewCh),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,
    Ch2 = channel_block_tx:update(Ch, A, N),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    ToAmount = case keys:id() of
            Acc1 -> true = A > 0, 1;
            Acc2 -> true = A < 0, 0
        end,
    SecretHash = language:extract_sh(hd(channel_block_tx:bets(NewCh))),
    Script = language:hashlock(ToAmount, SecretHash),
    NewCh = channel_block_tx:add_bet(Ch2, A, Script),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce and bet.
    store(ChId, NewCh).
    
recieve(ChId, MinAmount, NewCh) ->
    channel_block_tx:is_cb(NewCh),
    F = read(ChId),
    Ch = F#f.channel,
    NewAmount = channel_block_tx:amount(NewCh),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(NewCh),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,
    NewCh = channel_block_tx:update(Ch, A, N),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce.
    BTA1C = block_tree:acc1(Channel),
    BTA2C = block_tree:acc2(Channel),
    B = case keys:id() of
        BTA1C -> A;
        BTA2C -> -A
    end,
    true = B > MinAmount - 1,
    store(ChId, NewCh).
    
    
    
