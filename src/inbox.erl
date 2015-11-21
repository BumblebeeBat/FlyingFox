%This module keeps track of messages you receive.
-module(inbox).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, peers/0,msg_ids/1,read/2,delete/2,delete/1,get/1,test/0]).
-record(f, {next = 0, msgs = dict:new()}).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Id}, X) -> 
    NewX = case dict:find(Id, X) of
               error -> X;
               {ok, _} ->
                   dict:erase(Id, X)
           end,
    {noreply, NewX};
handle_cast({delete, Id, Index}, X) -> 
    NewX = case dict:find(Id, X) of
        error -> X;
        {ok, A} ->
            case dict:find(Index, A#f.msgs) of
                error -> X;
                {ok, _} ->
                    C = dict:erase(Index, A#f.msgs),
                    dict:store(Id, #f{msgs = C, next = A#f.next}, X)
            end
    end,
    {noreply, NewX};
handle_cast({get, Id, Msg}, X) ->
    F = case dict:find(Id, X) of
            error -> #f{};
            {ok, Z} -> Z
        end,
    N = F#f.next,
    D = dict:store(N, Msg, F#f.msgs),
    NewX = dict:store(Id, #f{next = N+1, msgs = D}, X),
    {noreply, NewX}.
handle_call(peers, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call({msg_ids, Id}, _From, X) -> 
    B = case dict:find(Id, X) of
            error -> no_peer;
            {ok, A} -> dict:fetch_keys(A#f.msgs)
        end,
    {reply, B, X};
handle_call({read, Id, Index}, _From, X) -> 
    B = case dict:find(Id, X) of
        error -> no_peer;
        {ok, A} -> 
                case dict:find(Index, A#f.msgs) of
                    error -> no_message;
                    {ok, C} -> C
                end
        end,
    {reply, B, X}.

delete(Id, Index) -> gen_server:cast(?MODULE, {delete, Id, Index}).
delete(Id) -> gen_server:cast(?MODULE, {delete, Id}).
get(Msg) -> 
    M = encryption:get_msg(Msg),
    FromPub = encryption:from(M),
    FromId = encryption:id(M),
    Peer = block_tree:account(FromId),
    FromPub = accounts:pub(Peer),
    get_helper(FromId, encryption:msg(M)).
get_helper(X, Y) -> gen_server:cast(?MODULE, {get, X, Y}).
peers() -> gen_server:call(?MODULE, peers).
msg_ids(Id) -> gen_server:call(?MODULE, {msg_ids, Id}).
read(Id, Index) -> gen_server:call(?MODULE, {read, Id, Index}).
    
test() ->
    Peer = 1,
    get_helper(Peer, "hello"),
    get_helper(Peer, "hello2"),
    get_helper(Peer, "hello3"),
    P = peers(),
    P = [Peer],
    X = msg_ids(Peer),
    io:fwrite(packer:pack(X)),
    X = [0,2,1],
    H = read(Peer, 0),
    H = "hello",
    delete(Peer, 0),
    Y = msg_ids(Peer),
    Y = [2,1],
    delete(Peer),
    P2 = peers(),
    P2 = [],
    success.
                    
    
    
    
    
