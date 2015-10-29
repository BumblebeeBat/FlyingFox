%this module needs to keep track of the highest-nonced transaction recieved in each channel.

-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, unlock_hash/2,hashlock/3,spend/2,recieve/3,read/1,new_store/2]).
-record(f, {channel = [], unlock = []}).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, N, Ch}, X) -> 
    {noreply, dict:store(N, Ch, X)}.
handle_call({read, N}, _From, X) -> 
    {reply, dict:fetch(N, X), X}.
repeat(Times, X) -> repeat(Times, X, []).
repeat(0, _, Out) -> Out;
repeat(Times, X, Out) -> repeat(Times-1, X, [X|Out]).
    
store(ChId, F) -> gen_server:cast(?MODULE, {store, ChId, F}).

new_store(ChId, Ch) -> 
    F = #f{channel = Ch, unlock = repeat(length(channel_block_tx:bets(Ch)), 28)},
    store(ChId, F).

read(ChId) -> gen_server:call(?MODULE, {read, ChId}).
spend(ChId, Amount) ->
    F = read(ChId),
    Ch = F#f.channel,
    NewCh = channel_block_tx:update(Ch, Amount, 1),
    NewF = #f{channel = NewCh, unlock = F#f.unlock},
    store(ChId, NewF).
recieve(ChId, MinAmount, NewCh) ->
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
    

hashlock(ChId, Amount, SecretHash) ->
    0.
unlock_hash(ChId, Secret) ->
    0.
    
