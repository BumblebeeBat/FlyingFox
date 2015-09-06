-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/5,top/0,read/1,account/2,channel/2,keys/0]).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {accounts = 0, channels = 0, block = 0, parent = finality}).
%-record(signed, {data="", sig="", sig2="", revealed=[]}).
init(ok) -> 
    D = dict:new(),
    %TopBlock = block_finality:top(),
    %BH = hash:doit(TopBlock#signed.data),
    %Top = #x{accounts = A, channels = C, block = TopBlock},
    E = dict:store(top, finality, D),
    %F = dict:store(BH, Top, E),
    {ok, E}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block digest died!"), ok.
handle_info(_, X) -> {noreply, X}.
%handle_call(keys, _From, D) -> {reply, dict:fetch_keys(D), D};
handle_call(top, _From, D) -> 
    X = dict:fetch(top, D),
    Y = if
            X == finality -> block_finality:top();
            true -> dict:fetch(top, D)
        end,
%if it is finality, block_finality:top()
{reply, Y, D};
handle_call(keys, _From, D) -> {reply, dict:keys(D), D};
handle_call({read, finality}, _From, D) -> {reply, block_finality:top(), D};
handle_call({read, V}, _From, D) -> 
    F = dict:find(V, D),
    case F of
        {ok, Value} -> {reply, Value, D};
        error -> {reply, "none", D}
    end.
handle_cast({write, K, V}, D) -> 
    Signed = read(top()),
    CurrentTop = Signed#signed.data#block.height,
    NewHeight = V#x.block#signed.data#block.height,
    ND = if
        NewHeight > CurrentTop -> 
                 %possible pruning, and merge digests into finality.
                 txs:dump(),
                 dict:store(top, hash:doit(V#x.block#signed.data));
        true -> D
    end,
    {noreply, dict:store(K, V, ND)}.
top() -> gen_server:call(?MODULE, top).
keys() -> gen_server:call(?MODULE, keys).
read(K) -> gen_server:call(?MODULE, {read, K}).
write(Key, AccountsDict, ChannelsDict, ParentKey, SignedBlock) ->
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    gen_server:cast(?MODULE, {write, Key, V}).
account(N, finality) -> finality_accounts:read(N);
account(N, H) ->
    X = read(H),
    Accounts = X#x.accounts,
    Parent = X#x.parent,
    case dict:find(N, Accounts) of
        error -> account(N, Parent);
        {ok, Val} -> 
            <<Balance:48, Nonce:32, P/binary>> = Val,
            {P, Nonce, Balance}
    end.
channel(N, finality) -> finality_channels:read(N);
channel(N, H) ->
    X = read(H),
    Channels = X#x.channels,
    Parent = X#x.parent,
    case dict:find(N, Channels) of
        error -> channel(N, Parent);
        {ok, Val} -> Val
    end.

test() -> 0.
%S = #signed{data = {}},
 %   write([S]).

    
%this file should be block_tree.erl
