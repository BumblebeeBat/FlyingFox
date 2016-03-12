-module(channel_manager_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, recieve/3,recieve_account/3,channel/1,recieve_locked_payment/4,spend_locked_payment/4,spend/2,new_channel/3,create_unlock_hash/2,spend_account/2,read_channel/1,unlock_hash/4]).
-record(f, {channel = [], unlock = []}).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({new_channel, ChId, Channel, Accounts}, _From, X) ->
    Ch = channel_block_tx:channel_block_from_channel(ChId, Channel, 0, 1, constants:max_reveal()-1, 0, [], Accounts),
    F = #f{channel = Ch, unlock = []},
    Out = channel_manager:store(ChId, F),
    {reply, Out, X};
handle_call({locked_payment, ChId, SignedChannel, Amount, SecretHash, Spend}, _From, X) ->
    NewCh = sign:data(SignedChannel),
    true = channel_block_tx:is_cb(NewCh),
    F = channel_manager:read(ChId),
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
    io:fwrite("A is: "),
    io:fwrite(integer_to_list(A)),
    io:fwrite("\n"),
    io:fwrite("Amount is: "),
    io:fwrite(integer_to_list(Amount)),
    io:fwrite("\n"),
    ToAmount = 
	case ID of
	    Acc1 ->
		true = (A == (Amount div 2)),
		if 
		    Spend -> 1; 
		    true -> 
			true = A > 0,
			0 
		end;
	    Acc2 ->
		true = (-A == (Amount div 2)),
		if 
		    Spend -> 2; 
		    true -> 
			true = A < 0,
			1 
		end
	end,
    SecretHash = language:extract_sh(channel_block_tx:bet_code(hd(channel_block_tx:bets(NewCh)))),
    Script = language:hashlock(SecretHash),
    NewCha = channel_block_tx:add_bet(Ch2, A, Script, ToAmount),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce and bet.
    io:fwrite("NewCha is "),
    io:fwrite(packer:pack(NewCha)),
    io:fwrite("\n"),
    io:fwrite("NewCh is "),
    io:fwrite(packer:pack(NewCh)),
    io:fwrite("\n"),
    NewCh = NewCha,
    NewF = #f{channel = SignedChannel, unlock = [[28]|F#f.unlock]},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(NewCh),
    {reply, Out, X};
handle_call({unlock_hash, ChId, Secret, SignedCh, BH}, _From, X) ->
    {SignedCh2, N, BetCode} = common(ChId, Secret),
    io:fwrite("Bet "),
    io:fwrite(packer:pack(BetCode)),
    io:fwrite("\n"),
    BH = hash:doit(BetCode),
    NewCh = sign:data(SignedCh2),
    NewCh = sign:data(SignedCh),
    F = channel_manager:read(ChId),
    NewUnlock = replace_n(N, Secret, F#f.unlock),
    %channel_block_tx:add,
    NewF = #f{channel = SignedCh, unlock = NewUnlock},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(NewCh),
    {reply, Out, X};
handle_call({recieve, ID, Payment, MinAmount, ChId, SignedPayment}, _From, X) -> 
    F = channel_manager:read(ChId),
    Ch = sign:data(F#f.channel),
    NewAmount = channel_block_tx:amount(Payment),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(Payment),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,
    Payment2 = channel_block_tx:update(Ch, A, N),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce.
    Payment = Payment2,
    %recieve. positive goes to 1.
    BTA1C = channels:acc1(Channel),
    BTA2C = channels:acc2(Channel),
    B = case ID of
        BTA1C -> A;
        BTA2C -> -A
    end,
    true = B > MinAmount - 1,
    NewF = #f{channel = SignedPayment, unlock = F#f.unlock},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(Payment),
    {reply, Out, X};
handle_call(_, _From, X) -> {reply, X, X}.

recieve_account(Acc, MinAmount, SignedPayment) ->
    recieve(hd(channel_manager:id(Acc)), MinAmount, SignedPayment).
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
    gen_server:call(?MODULE, {recieve, ID, Payment, MinAmount, ChId, SignedPayment}).
channel(X) -> X#f.channel.
read_channel(Key) ->
    F = channel_manager:read(Key),
    sign:data(F#f.channel).
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

common(ChId, Secret) ->
    SecretHash = hash:doit(Secret),
    OldCh = read_channel(ChId),
    Bets = channel_block_tx:bets(OldCh),
    N = match_n(SecretHash, Bets),%if the bets were numbered in order, N is the bet we are unlocking.
    Bet = nth(N, Bets),
    A = channel_block_tx:bet_amount(Bet),
    BetCode = channel_block_tx:bet_code(Bet),
    ToFlip = channel_block_tx:bet_to(Bet),
    Amount = language:valid_secret(Secret, BetCode),
    NewBets = remove_nth(N, Bets),
    NewCh = channel_block_tx:replace_bet(OldCh, NewBets),
    C = if 
	    ToFlip == 0 -> fractions:subtract({f, 1, 1}, Amount);
	    true -> Amount
	end,
    D = fractions:multiply_int(C, A),
    NewNewCh = channel_block_tx:update(NewCh, D, 1),%
    %we need to change amount.
    {keys:sign(NewNewCh), N, BetCode}.
create_unlock_hash(ChId, Secret) ->
    {SignedCh, _, _} = common(ChId, Secret),
    SignedCh.
nth(0, [X|_]) -> X;
nth(N, [_|T]) -> nth(N-1, T).
unlock_hash(ChId, Secret, SignedCh, BH) ->
    gen_server:call(?MODULE, {unlock_hash, ChId, Secret, SignedCh, BH}).
general_locked_payment(ChId, SignedChannel, Amount, SecretHash, Spend) ->
    gen_server:call(?MODULE, {locked_payment, ChId, SignedChannel, Amount, SecretHash, Spend}).
recieve_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, false).
spend_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, true),
    ok.
new_channel(ChId, Channel, Accounts) -> 
    gen_server:call(?MODULE, {new_channel, ChId, Channel, Accounts}).
spend_account(Acc, Amount) ->
    spend(hd(channel_manager:id(Acc)), Amount).
spend(ChId, Amount) ->
    Ch = read_channel(ChId),
    A1 = channel_block_tx:acc1(Ch),
    A2 = channel_block_tx:acc2(Ch),
    A = case keys:id() of
	    A1 -> -Amount;
	    A2 -> Amount
	end,
    keys:sign(channel_block_tx:update(Ch, A, 1)).

