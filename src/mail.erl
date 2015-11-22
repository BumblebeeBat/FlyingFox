-module(mail).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, pop/1,pop_maker/1,cost/2,register/2,send/3,pop/0,status/0,test/0,register_cost/0,internal_send/3]).
-record(msg, {start, lasts, msg, size = 0, to}).
-record(d, {db = dict:new(), accs = 0, msgs = 0}).
init(ok) -> {ok, #d{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new, Acc}, X) -> 
    B = case dict:find(Acc, X#d.db) of
            error ->
                NewD = dict:store([], Acc, X#d.db),
                #d{db = NewD, accs = X#d.accs, msgs = X#d.msgs};
            {ok, _} -> X
    end,
    {noreply, B};
handle_cast({send, To, Message, Seconds}, X) -> 
    Accs = X#d.accs,
    DB = X#d.db,
    Msg = #msg{msg = Message, start = now(), lasts = Seconds, size = size(Message), to = To},%price is the rate at which this costs money?
    A = case dict:find(To, DB) of
            error -> [];
            {ok, Val} -> Val
    end,
    NewD = dict:store(To, [Msg|A], DB),
    NewX = #d{db = NewD, accs = Accs, msgs = X#d.msgs + 1},
    {noreply, NewX}.
handle_call({pop, Acc}, _From, X) -> 
    {Out, NewX} = 
        case dict:find(Acc, X#d.db) of
            error -> {empty, X};
            {ok, []} -> {empty, X};
            {ok, Val} ->
                D = dict:store(Acc, tl(Val), X#d.db),
                NX = #d{db = D, accs = X#d.accs, msgs = X#d.msgs-1},
                {hd(Val), NX}
        end,
    {reply, Out, NewX};
handle_call(status, _From, X) -> {reply, X#d.db, X}.
-define(POP, <<1,7,3,24,7,4,2>>).
%price(Accounts, Messages) -> 10000 + ((Accounts + Messages) * 100).
pop() -> ?POP.
pop_maker(To) ->
    %Acc = keys:id(),
    A = block_tree:account(To),
    Pub = accounts:pub(A),
    encryption:send_msg(nonce:server_get(To), Pub).
pop(M) ->
    E = encryption:get_msg(M),
    From = encryption:id(E),
    Nonce = nonce:customer_get(From),
    Nonce = encryption:msg(E),
    %block_tree:account(From),
    %SmallTime = abs(timer:now_diff(now(), Time) div 1000000),
    %true = SmallTime < 10,
    %H = << Time/binary, ?POP/binary >>,
    %A = block_tree:account(From),
    %Pub = accounts:pub(A),
    %true = sign:verify_sig(H, Sig, Pub),
    pop2(From).
pop2(From) ->
    M = gen_server:call(?MODULE, {pop, From}),
    case M of
	empty -> "no more messages";
	X -> pop3(From, X)
    end.
pop3(From, M) ->
    io:fwrite("pop2 M "),
    io:fwrite(packer:pack(M)),
    io:fwrite("\n"),
    Msg = M#msg.msg,
    T = timer:now_diff(now(), M#msg.start) + 2000000,%2 second fee automatically.
    Cost = cost(size(Msg), M#msg.lasts),
    R = (M#msg.lasts * 1000000),
    Refund = ((R - T) * Cost) div R,
    if
        Refund < 1 -> 
	    io:fwrite("you paid for seconds "),
	    io:fwrite(integer_to_list(M#msg.lasts)),
	    io:fwrite("\n"),
	    io:fwrite("you needed"),
	    io:fwrite(integer_to_list(T)),
	    io:fwrite("\n"),
	    pop2(From);%Acc, Sig, Time);
        true -> 
	    nonce:customer_next(From),
	    {pop_response, Msg, channel_manager:spend_account(From, Refund)}
    end.
cost(MsgSize, Time) -> 10000 * MsgSize * Time. %time in seconds
-define(REGISTER, 100000).
register_cost() -> ?REGISTER.
status() -> gen_server:call(?MODULE, status).
register(Payment, Acc) ->
    ChId = hd(channel_manager:id(Acc)),
    channel_manager:recieve(ChId, ?REGISTER, Payment),
    gen_server:cast(?MODULE, {new, Acc}).
send(To, Msg, Seconds) ->
    gen_server:cast(?MODULE, {send, To, Msg, Seconds}).
internal_send(To, Msg, Seconds) ->
    send(To, Msg, Seconds).
%delete_account(Acc, Sig) ->
%    Time = abs(timer:now_diff(now(), M#msg.time) div 1000000),
%    true = Time < 10,%time must be within 10 seconds of now
    %Sig must be over Time appended with <<"delete account">>.
%    ok = gen_server:call(?MODULE, {del_acc, Acc}),
%    channel_manager:spend_acc(Acc, ?REGISTER div 10 * 9).

            
test() ->            
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 620000, 0),
    spend_tx:spend(1, 10, 0),
    sign_tx:sign(),
    reveal:reveal(),
    block_tree:buy_block(),
    CreateTx1 = to_channel_tx:create_channel(3, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx1 = sign:sign_tx(CreateTx1, Pub, Priv, tx_pool:accounts()),
    tx_pool:absorb(SignedCreateTx1),
    sign_tx:sign(),
    reveal:reveal(),
    block_tree:buy_block(),
    gen_server:cast(?MODULE, {new, 3}),
    Msg = <<"test">>,
    gen_server:cast(?MODULE, {send, 3, Msg, 0}),
    gen_server:cast(?MODULE, {send, 3, Msg, 0}),
    Out = gen_server:call(?MODULE, {pop, 3}),
    Msg = Out#msg.msg,
    success.
