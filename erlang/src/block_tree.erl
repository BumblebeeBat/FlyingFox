-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,account/2,account/3,channel/2,absorb/1,is_key/1]).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {block = 0, parent = finality, accounts = dict:new(), channels = dict:new()}).
init(ok) -> 
    SignedBlock = block_finality:top_block(),
    X = #x{block = SignedBlock},
    BH = hash:doit(SignedBlock#signed.data),
    D = dict:store(top, BH, dict:new()),
    E = dict:store(BH, X, D),%store blocks by hash
    {ok, E}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(top, _From, D) -> 
    {reply, dict:fetch(dict:fetch(top, D), D), D};
handle_call({key, X}, _From, D) -> {reply, dict:is_key(X, D), D};
%handle_call({read, finality}, _From, D) -> {reply, block_finality:top(), D};%shouldn't just be top...
handle_call({read, V}, _From, D) -> 
    F = dict:find(V, D),
    case F of
        {ok, Value} -> {reply, Value, D};
        error -> {reply, "none", D}
    end.
handle_cast({write, K, V}, D) -> 
    Top = dict:fetch(dict:fetch(top, D), D),
    TopHeight = Top#x.block#signed.data#block.height,
    NewHeight = V#x.block#signed.data#block.height,
    ND = if
        NewHeight > TopHeight -> 
                 %possible pruning, and merge digests into finality.
                 txs:dump(),
                 dict:store(top, hash:doit(V#x.block#signed.data), D);
        true -> D
    end,
    {noreply, dict:store(K, V, ND)}.
top() -> gen_server:call(?MODULE, top).
is_key(X) -> gen_server:call(?MODULE, {key, X}).
read(K) -> gen_server:call(?MODULE, {read, K}).
write(SignedBlock) ->
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = is_key(BH),
    ParentKey = Block#block.hash,
    Parent = read(ParentKey),%"undefined"
    %io:fwrite(Parent),
    io:fwrite("\n"),
    OldHeight = Parent#x.block#signed.data#block.height,%we need to add more if this skipped height. 
    NewHeight = Block#block.height,
    true = NewHeight > OldHeight,
%were validated by enough signers,
%check that the amount bonded is sufficiently big compared to the amount being spent, and the size of the block.
    Size = size(packer:pack(Block)),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    io:fwrite("block tree write\n"),
    io:fwrite(packer:pack(Block#block.txs)),
    io:fwrite("\n"),
    {AccountsDict, ChannelsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new()),
%give out rewards for validators in the digest.
%take fee from block creator in the digest.
    %make sure there is no negative money

    Key = hash:doit(SignedBlock#signed.data),
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    gen_server:cast(?MODULE, {write, Key, V}).
absorb([]) -> [];
absorb([Block|T]) -> [write(Block)|absorb(T)].
account(N, AccountsDict) ->
    %H = read(top),
    account(N, read(top), AccountsDict).
account(N, H, AccountsDict) ->
    B = dict:is_key(N, AccountsDict),
    if
        B -> dict:fetch(N, AccountsDict);
        true -> account_helper(N, H)
    end.
account_helper(N, finality) -> finality_accounts:read_account(N);
account_helper(N, H) ->
    X = read(H),%none
    %io:fwrite(packer:pack(X)),
    Accounts = X#x.accounts,
    Parent = X#x.parent,
    case dict:find(N, Accounts) of
        error -> account_helper(N, Parent);
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
-record(spend, {from=0, to=0, nonce = 0, amount=0}).
test() -> 
    io:fwrite("test\n"),
    Tx = #spend{from = 0, to = 1, amount=10},
    Txs = [keys:sign(Tx)],
    SignedParent = block_finality:top_block(),
    Parent = SignedParent#signed.data,
    PHash = hash:doit(Parent),
    Block = #block{txs = Txs, hash = PHash, height = 1},
    SignedBlock = #signed{data = Block},
    io:fwrite(packer:pack(SignedBlock)),
    io:fwrite("\n signed block ^^^ \n"),
    absorb([SignedBlock]).
