-module(internal_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", packer:pack({pubkey})}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, D, Req),
    {ok, Req2, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
-define(POP, <<1,6,3,87,3,5>>).
doit({create_account, Pub, Amount, Fee}) -> 
    create_account_tx:create_account(Pub, Amount, Fee);
doit({spend, To, Amount, Fee}) ->
    spend_tx:spend(To, Amount, Fee);
doit({buy_block}) -> block_tree:buy_block();
doit({sign_block}) -> sign_tx:sign();
doit({sign, Tx}) -> {ok, keys:sign(Tx)};
doit({create_channel, Partner, Bal1, Bal2, Type, Fee}) ->
    to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee);
doit({to_channel, ChId, Inc1, Inc2, Fee}) ->
    to_channel_tx:to_channel(ChId, Inc1, Inc2, Fee);
doit({close_channel, ChId, Amount, Nonce, Fee}) ->
    channel_block_tx:close_channel(ChId, Amount, Nonce, Fee);
doit({sync, IP, Port}) ->
    io:fwrite("internal handler sync\n"),
    download_blocks:sync(IP, Port);
doit({pubkey}) -> {ok, keys:pubkey()};
doit({id}) -> {ok,  keys:id()};
doit({channel_id, Partner}) -> {ok, channel_manager:id(Partner)};
doit({new_pubkey, Password}) -> 
    io:fwrite("internal handler new pubkey\n"),
    keys:new(Password);
doit({channel_spend, ChId, Amount}) ->
    {ok, channel_manager:spend(ChId, Amount)};
doit({hashlock, ChId, Amount, SecretHash}) ->
    {ok, channel_manager:hashlock(ChId, Amount, SecretHash)};
doit({spend_locked_payment, ChId, SignedChannel}) ->
    %to spend, first use hashlock and send to peer. Your peer's responce is SignedChannel.
    {ok, channel_manager:spend_locked_payment(ChId, SignedChannel)};
doit({test}) -> 
    {test_response};
doit({get_msg, IP, Port}) ->
    Time = now() + 10000000,
    H = << Time/binary, ?POP/binary >>,
    Sig = keys:raw_sign(H),
    Msg = {pop, keys:id(), Sig, Time},
    M = talker:talk(Msg, IP, Port),
    {ok, inbox:get(M)};
doit({read_msg, Id, Index}) -> {ok, inbox:read(Id, Index)};
doit({msg_ids, Id}) -> {ok, inbox:msg_ids(Id)};
doit({msg_peers}) -> {ok, inbox:peers()};
doit({msg_delete, Id, Index}) -> {ok, inbox:delete(Id, Index)};
doit({msg_delete, Id}) -> {ok, inbox:delete(Id)};
doit({register, IP, Port}) ->
    PeerId = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    Amount = talker:talk({register_cost}, IP, Port),
    Payment = channel_manager:spend(ChId, Amount),
    Msg = {register, Payment, keys:id()},
    talker:talk(Msg, IP, Port),
    {ok, ok};
doit({send_msg, IP, Port, Amount, To, Msg, Seconds}) ->
    PeerId = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    Payment = channel_manager:spend(ChId, Amount),
    M = {send, Payment, To, Msg, Seconds},
    talker:talk(M, IP, Port),
    {ok, ok};
doit({new_channel, IP, Port, Bal1, Bal2, Fee}) ->
    Partner = talker:talk({id}, IP, Port),
    Type = <<"delegated_2">>,
    Tx = keys:sign(to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee)),
    Msg = {new_channel, Tx},
    Ch = talker:talk(Msg, IP, Port),
    tx_pool:absorb(Ch),
    {ok, ok};
doit({channel_spend, IP, Port, Partner, Amount}) ->
    PeerId = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    SecretHash = secrets:new(),
    Payment = channel_manager:hashlock(ChId, Amount, SecretHash),
    talker:talk({channel_locked_payment, ChId, Payment, Partner}, IP, Port),
    Acc = block_tree:account(Partner),
    Msg = encryption:send_msg(SecretHash, accounts:pub(Acc)),
    Seconds = 30,
    Cost = mail:cost(Msg, Seconds),
    MsgPayment = channel_manager:spend(ChId, Cost),
    

    talker:talk({send, MsgPayment, Partner, Msg, Seconds}, IP, Port),
    {ok, SecretHash};
doit({channel_unlock, IP, Port, Partner, Secret}) ->
    PeerId = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    UH = channel_manager:create_unlock_hash(ChId, Secret),
    NewCh = talker:talk({unlock, ChId, Secret, UH}, IP, Port),
    channel_manager:unlock_hash(ChId, Secret, NewCh),
    {ok, ok};
doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {error}.
