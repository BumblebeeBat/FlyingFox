%this module needs to keep track of the highest-nonced transaction recieved in each channel.

%We need the ability to spend and receive money, and to spend and receive hashlocked money.

-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, unlock_hash/3,hashlock/3,spend/2,recieve/3,read/1,new_channel/2,recieve_locked_payment/4,spend_locked_payment/4,delete/1,id/1,keys/0,create_unlock_hash/2,spend_account/2,recieve_account/3,test/0]).
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
    {reply, dict:fetch_keys(X), X}.
%repeat(Times, X) -> repeat(Times, X, []).
%repeat(0, _, Out) -> Out;
%repeat(Times, X, Out) -> repeat(Times-1, X, [X|Out]).
keys() -> gen_server:call(?MODULE, keys).
id(Partner) -> id_helper(Partner, keys(), []).
id_helper(_, [], Out) -> Out;
id_helper(Partner, [Key|T], Out) ->
    F = read(Key),
    Ch = sign:data(F#f.channel),
    Acc1 = channel_block_tx:acc1(Ch),
    Acc2 = channel_block_tx:acc2(Ch),
    NewOut = if
                 ((Partner == Acc1) or (Partner == Acc2)) -> [Key|Out];
                 true -> Out
	     end,
    id_helper(Partner, T, NewOut).

store(ChId, F) -> 
    gen_server:cast(?MODULE, {store, ChId, F}).

new_channel(ChId, Channel) -> 
    Ch = channel_block_tx:channel_block_from_channel(ChId, Channel, 0, 1, constants:max_reveal()-1, 0, []),
    F = #f{channel = Ch, unlock = []},
    store(ChId, F).

is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T).

read(ChId) -> 
    K = keys(),
    true = is_in(ChId, K),
    gen_server:call(?MODULE, {read, ChId}).
delete(ChId) -> gen_server:call(?MODULE, {delete, ChId}).
spend_account(Acc, Amount) ->
    spend(hd(id(Acc)), Amount).
spend(ChId, Amount) ->
    F = read(ChId),
    Ch = sign:data(F#f.channel),
    keys:sign(channel_block_tx:update(Ch, Amount, 1)).
match_n(X, Bets) -> match_n(X, Bets, 0).
match_n(X, [Bet|Bets], N) ->
    Y = language:extract_sh(channel_block_tx:bet_code(Bet)),
    if
        X == Y -> N;
        true -> match_n(X, Bets, N+1)
    end.
replace_n(N, New, L) -> replace_n(N, New, L, []).
replace_n(0, New, [_|L], Out) -> 
    lists:reverse(Out) ++ [New] ++ L;
replace_n(N, New, [H|L], Out) -> 
    replace_n(N-1, New, L, [H|Out]).
remove_nth(N, Bets) -> remove_nth(N, Bets, []).
remove_nth(0, [_|Bets], Out) -> lists:reverse(Out) ++ Bets;
remove_nth(N, [B|Bets], Out) -> remove_nth(N, Bets, [B|Out]).
create_unlock_hash(ChId, Secret) ->
    {SignedCh, _, _} = common(ChId, Secret),
    SignedCh.
nth(0, [X|_]) -> X;
nth(N, [_|T]) -> nth(N-1, T).
common(ChId, Secret) ->
    SecretHash = hash:doit(Secret),
    F = read(ChId),
    OldCh = sign:data(F#f.channel),
    Bets = channel_block_tx:bets(OldCh),
    N = match_n(SecretHash, Bets),%if the bets were numbered in order, N is the bet we are unlocking.
    Bet = nth(N, Bets),
    A = channel_block_tx:bet_amount(Bet),
    BetCode = channel_block_tx:bet_code(Bet),
    Amount = language:valid_secret(Secret, BetCode),
    NewBets = remove_nth(N, Bets),
    NewCh = channel_block_tx:replace_bet(OldCh, NewBets),
    NewNewCh = channel_block_tx:update(NewCh, Amount * A, 1),%
    %we need to change amount.
    {keys:sign(NewNewCh), N, BetCode}.
unlock_hash(ChId, Secret, SignedCh) ->
    {SignedCh2, N, BetCode} = common(ChId, Secret),
    NewCh = sign:data(SignedCh2),
    NewCh = sign:data(SignedCh),
    F = read(ChId),
    NewUnlock = replace_n(N, Secret, F#f.unlock),
    %channel_block_tx:add,
    NewF = #f{channel = SignedCh, unlock = NewUnlock},
    store(ChId, NewF),
    arbitrage:del(BetCode, ChId),
    keys:sign(NewCh).
hashlock(ChId, Amount, SecretHash) ->
    F = read(ChId),
    Ch = sign:data(F#f.channel),
    Ch2 = channel_block_tx:update(Ch, Amount div 2, 1),
    Channel = block_tree:channel(ChId),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    MyAccount = case keys:id() of
            Acc1 -> 1;
            Acc2 -> 0
        end,
    Script = language:hashlock(MyAccount, SecretHash),
    keys:sign(channel_block_tx:add_bet(Ch2, Amount div 2, Script)).
%NewF = #f{channel = Ch3, unlock = [[1]|F#f.unlock]},
%store(ChId, NewF).
recieve_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, false).
spend_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, true),
    ok.
general_locked_payment(ChId, SignedChannel, Amount, SecretHash, Spend) ->
    NewCh = sign:data(SignedChannel),
    true = channel_block_tx:is_cb(NewCh),
    F = read(ChId),
    Ch = sign:data(F#f.channel),
    NewAmount = channel_block_tx:amount(NewCh),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(NewCh),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,%error here.
    Ch2 = channel_block_tx:update(Ch, A, N),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    ID = keys:id(),
    ToAmount = if
	Spend -> 
	    case ID of Acc1 -> 1; Acc2 -> 0 end;
	true ->
	    case ID of
		%Acc1 -> true = A > 0, 0;
		%Acc2 -> true = A < 0, 1
		Acc1 -> true = A == (Amount div 2), 0;
		Acc2 -> true = A == (-Amount div 2), 1
	    end
    end,
    SecretHash = language:extract_sh(channel_block_tx:bet_code(hd(channel_block_tx:bets(NewCh)))),
    Script = language:hashlock(ToAmount, SecretHash),
    NewCha = channel_block_tx:add_bet(Ch2, A, Script),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce and bet.
    NewCh = NewCha,
    NewF = #f{channel = SignedChannel, unlock = [[28]|F#f.unlock]},
    store(ChId, NewF),
    keys:sign(NewCh).

recieve_account(Acc, MinAmount, SignedPayment) ->
    recieve(hd(id(Acc)), MinAmount, SignedPayment).
recieve(ChId, MinAmount, SignedPayment) ->
    %we need to verify that the other party signed it.
    ID = keys:id(),
    Payment = sign:data(SignedPayment),
    A1 = channel_block_tx:acc1(Payment),
    A2 = channel_block_tx:acc2(Payment),
    Acc1 = block_tree:account(A1),
    Acc2 = block_tree:account(A2),
    Pub1 = accounts:pub(Acc1),
    Pub2 = accounts:pub(Acc2),
    true = case ID of
               A1 -> sign:verify_2(SignedPayment, Pub2);
               A2 -> sign:verify_1(SignedPayment, Pub1)
    end,
    true = channel_block_tx:is_cb(Payment),
    F = read(ChId),
    Ch = sign:data(F#f.channel),
    NewAmount = channel_block_tx:amount(Payment),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(Payment),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,
    Payment = channel_block_tx:update(Ch, A, N),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce.
    BTA1C = channels:acc1(Channel),
    BTA2C = channels:acc2(Channel),
    B = case ID of
        BTA1C -> A;
        BTA2C -> -A
    end,
    true = B > MinAmount - 1,
    NewF = #f{channel = SignedPayment, unlock = F#f.unlock},
    store(ChId, NewF),
    keys:sign(Payment).
    
test() ->    
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 620000, 0),
    Partner = 4,
    spend_tx:spend(Partner, 10, 0),
    sign_tx:sign(),
    reveal:reveal(),
    block_tree:buy_block(),
    CreateTx1 = to_channel_tx:create_channel(Partner, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx1 = sign:sign_tx(CreateTx1, Pub, Priv, tx_pool:accounts()),
    tx_pool:absorb(SignedCreateTx1),
    sign_tx:sign(),
    reveal:reveal(),
    block_tree:buy_block(),

    %T = keys(),
    S = 24005,
    %T = [S],
    C = read(S),
    D = element(2, element(2, C)),
    D = {channel_block,0,Partner,0,1,[],S,false,259,0,0,0},

    %Example of spending money through a channel.
    A = 1000,
    Tx = spend(S, A - 1),%send Tx to partner
    Tx2 = sign:sign_tx(Tx, Pub, Priv, tx_pool:accounts()),%partner runs recieve/3 with 2nd option set to 0, returns Tx2.
    recieve(S, -A, Tx2),

    %Example of spending to ID rather than ChId.
    B = 2000,
    Tx3 = spend_account(Partner, B - 1),
    Tx4 = sign:sign_tx(Tx3, Pub, Priv, tx_pool:accounts()),
    recieve_account(Partner, -B, Tx4),

    %example of hashlocked transaction.
    SH = secrets:new(),
    Amount = 200,
    Tx5 = hashlock(S, Amount, SH),
    Tx6 = sign:sign_tx(Tx5, Pub, Priv, tx_pool:accounts()),%partner runs recieve_locked_payment/3, and returns Tx6
    spend_locked_payment(S, Tx6, Amount, SH),%we absorb Tx6.

    Tx7 = create_unlock_hash(S, secrets:read(SH)),
    Tx8 = sign:sign_tx(Tx7, Pub, Priv, tx_pool:accounts()),%partner runs unlock_hash/3 and returns Tx8
    unlock_hash(S, secrets:read(SH), Tx8),
    E = element(2, element(2, read(S))),
    E = {channel_block,0,Partner,3198,5,[],S,false,259,0,0,0},
    success.



    
%success.
    %spend_locked_payment(24000, 
			 
			 
