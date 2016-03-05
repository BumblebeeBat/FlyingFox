-module(handler).

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    io:fwrite("handler got data "),
    io:fwrite(Data),
    io:fwrite("\n"),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, D, Req),
    {ok, Req2, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
-define(WORD, 10000000).%10 megabytes.
doit({pubkey}) -> {ok, keys:pubkey()};
doit({height}) -> {ok, block_tree:height()};
doit({give_block, SignedBlock}) -> 
    block_tree:absorb([SignedBlock]),
    {ok, 0};
doit({block, N}) -> 
    io:fwrite("handler doit block "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    {ok, block_tree:read_int(N)};
doit({tophash}) -> {ok, hash:doit(block_tree:top())};
doit({recent_hash, H}) -> {ok, block_tree:is_key(H)};
doit({accounts_size}) ->
    %{ok, filelib:file_size("backup/accounts.db") div ?WORD};
    {ok, filelib:file_size(constants:backup_accounts()) div ?WORD};
doit({tx_absorb, Tx}) -> 
    {ok, tx_pool_feeder:absorb(Tx)};
doit({accounts, N}) ->
    {ok, File} = file:open(constants:backup_accounts(), [read, binary, raw]),
    O = case file:pread(File, ?WORD * N, ?WORD) of
	    eof -> "eof";
	    {ok, Out} -> Out;
	    {error, Reason} -> 
		io:fwrite("file read error\n"),
		Reason
	end,
    file:close(File),
    {ok, O};
doit({channel_recieve, ChId, MinAmount, Ch}) ->
    Response = channel_manager_feeder:recieve(ChId, MinAmount, Ch),
    channel_partner:store(ChId, Response),
    {ok, Response};
doit({locked_payment, From, To, Payment, Amount, SecretHash}) ->
    ChIdFrom = hd(channel_manager:id(From)),
    Return = channel_manager_feeder:recieve_locked_payment(ChIdFrom, Payment, Amount, SecretHash),
    channel_partner:store(ChIdFrom, Return),
    ChIdTo = hd(channel_manager:id(To)),
    arbitrage:absorb(Payment, ChIdFrom, ChIdTo),
    Payment2 = channel_manager:new_hashlock(To, Amount, SecretHash),
    M = {locked_payment, Payment2, ChIdFrom, Amount, SecretHash},
    mail:internal_send(To, M, locked_payment),
    {ok, Return};
%locked_payment2 is the response from a message we sent to their mail box.
doit({locked_payment2, Payment, ChId, Amount, SecretHash}) ->
    channel_manager_feeder:spend_locked_payment(ChId, Payment, Amount, SecretHash),
    channel_partner:store(ChIdTo, Payment2),
    {ok, 0};
doit({txs}) -> {ok, tx_pool:txs()};
doit({txs, Txs}) -> 
    download_blocks:absorb_txs(Txs),
    {ok, 0};
doit({unlock, ChId, Secret, SignedCh}) ->
    Response = channel_manager_feeder:unlock_hash(ChId, Secret, SignedCh),
    arbitrage:check(Bet),
    %Now that one is unlocked, we should unlock the other. Maybe we should use mailbox? or maybe we should update the api, so they can lookup how the channel state updated?
    M = {unlock, Payment2, ChIdFrom, Amount, Secret},
    mail:internal_send(To, M, unlock),
    channel_partner:store(ChId, Response),
    {ok, Response};
doit({unlock2, Payment, ChId, Amount, SecretHash}) ->
    channel_manager_feeder:unlock_hash(ChId, Secret, SignedCh),
    
    {ok, 0};
doit({register, Payment, Acc}) ->
    {ok, mail:register(Payment, Acc)};
doit({channel_spend, Payment, Partner}) ->
    Response = channel_manager_feeder:recieve_account(Partner, 0, Payment),
    ChId = hd(channel_manager:id(Partner)),
    channel_partner:store(ChId, Response),
    {ok, Response};
doit({mail_cost, Space, Time}) ->
    {ok, mail:cost(Space, Time)};
doit({send, Payment, From, To, Msg, Seconds}) ->
    C = mail:cost(size(Msg), Seconds),
    ChId = channel_manager:id(From),
    R = channel_manager_feeder:recieve(ChId, C, Payment),
    channel_partner:store(ChId, R),
    mail:send(To, Msg, Seconds),
    {ok, R};
doit({id}) -> {ok, keys:id()};
doit({pop_hashes, Account}) ->
    {ok, mail:pop_hashes(Account)};
doit({pop, Account, Hashes}) ->
    {ok, mail:pop(Account, Hashes)};
doit({register_cost}) ->
    {ok, mail:register_cost()};
%need a way to share recent txs.			   
%I want to share the backup version of all the files.
doit({nonce, ID}) ->
    A = nonce:customer_get(ID),
    {ok, A};
doit({new_channel, SignedTx}) ->
    %make sure the Tx is more than 1/2 funded by the other person.
    %make sure the amount of money is below some limit.
    Tx = sign:data(SignedTx),
    io:fwrite("min ratio tx "),
    io:fwrite(packer:pack(Tx)),
    io:fwrite("\n"),
    true = to_channel_tx:min_ratio(free_constants:liquidity_ratio(), Tx),
    %true = to_channel_tx:half_them(Tx),
    MC = free_constants:max_channel(), 
    MS = to_channel_tx:my_side(Tx),
    true = MC > MS,
    NTx = keys:sign(SignedTx),
    tx_pool_feeder:absorb(NTx),
    {ok, NTx};
doit({update_channel, ISigned, TheySigned}) ->%The contents of this message NEED to be encrypted. ideally we should encypt every message to this module.
    %update_channel is a response when someone reads a message from their inbox and gets a refund for some of the money
    
    Data = sign:data(ISigned),
    Data = sign:data(TheySigned),
    true = sign:verify(keys:sign(TheySigned), tx_pool:accounts()),
    CA1 = channel_block_tx:acc1(Data),
    CA2 = channel_block_tx:acc2(Data),
    TheirId = 
	case keys:id() of
	    CA1 -> 
		true = sign:verify_1(ISigned, keys:pubkey()),
		CA2;
	    CA2 -> 
		true = sign:verify_2(ISigned, keys:pubkey()),
		CA1
	end,
    NewNonce = channel_block_tx:nonce(Data),
    %ChId = hd(channel_manager:id(TheirId)),
    %Data = sign:data(hd(channel_partner:read(ChId))),
    OldCh = channel_manager:read_channel(ChId),
    OldNonce = channel_block_tx:nonce(OldCh),
    true = NewNonce > OldNonce,
    channel_manager_feeder:recieve(ChId, -constants:initial_coins(), TheySigned),
    {ok, 0};
doit({to_channel, SignedTx}) ->
    Tx = sign:data(SignedTx),
    true = to_channel_tx:min_ratio(free_constants:liquidity_ratio(), Tx),
    tx_pool_feeder:absorb(keys:sign(SignedTx)),
    {ok, 0};
doit({backup_size, File}) ->
    io:fwrite("backup size handler"),
    io:fwrite(packer:pack(File)),
    io:fwrite("\n"),
    {ok, backup:read_size(File)};
doit({backup_read, File, N}) ->
    {ok, backup:read(File, N)};
doit(X) ->
    io:fwrite("I can't handle this \n"),
    io:fwrite(packer:pack(X)), %unlock2
    {error}.
    
