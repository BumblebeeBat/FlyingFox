-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,account/2,account/3,channel/2,channel/3,absorb/1,is_key/1,height/1]).
-record(block, {acc = 0, height = 0, hash = "", bond_size = 5000000, txs = []}).
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
height(K) -> 
    X = read(K),
    X#x.block#signed.data#block.height.
write(SignedBlock) ->
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = is_key(BH),
    ParentKey = Block#block.hash,
    Parent = read(ParentKey),
    OldHeight = Parent#x.block#signed.data#block.height,%we need to charge more if this skipped height. 
    NewHeight = Block#block.height,
    true = NewHeight > OldHeight,
%check that it was validated by enough signers,
%check that the amount bonded is sufficiently big compared to the amount being spent, and the size of the block.
    Size = size(packer:pack(Block)),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    {ChannelsDict, AccountsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new()),
%give out rewards for validators in the digest.
%take fee from block creator in the digest.

    Key = hash:doit(SignedBlock#signed.data),
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    gen_server:cast(?MODULE, {write, Key, V}).
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
account(N, AccountsDict) -> account(N, read(top), AccountsDict).
account(N, H, AccountsDict) ->
    B = dict:is_key(N, AccountsDict),
    if
        B -> dict:fetch(N, AccountsDict);
        true -> account_helper(N, H)
    end.
-record(acc, {balance = 0, nonce = 0, pub = 0}).
account_helper(N, finality) -> finality_accounts:read_account(N);
account_helper(N, H) ->
    X = read(H),
    Accounts = X#x.accounts,
    Parent = X#x.parent,
    case dict:find(N, Accounts) of
        error -> account_helper(N, Parent);
        {ok, Val} -> Val
    end.
channel(N, Channels) -> channel(N, read(top), Channels).
channel(N, H, Channels) ->
    B = dict:is_key(N, Channels),
    if
        B -> dict:fetch(N, Channels);
        true -> channel_helper(N, H)
    end.
channel_helper(N, finality) -> finality_channels:read_channel(N);
channel_helper(N, H) ->
    X = read(H),
    Channels = X#x.channels,
    Parent = X#x.parent,
    case dict:find(N, Channels) of
        error -> channel_helper(N, Parent);
        {ok, Val} -> Val
    end.
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(ca, {from = 0, nonce = 0, to = 0, pub = <<"">>, amount = 0}).
-record(cc, {acc1 = 0, nonce = 0, acc2 = 1, delay = 10, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
sign_all([]) -> [];
sign_all([Tx|Txs]) -> [keys:sign(Tx)|sign_all(Txs)].
test() -> 
    io:fwrite("test\n"),
    {Pub, _Priv} = sign:new_key(),
    Txs = sign_all(
	    [#ca{from = 0, nonce = 1, to=1, pub=Pub, amount=10},
	     #spend{from = 0, nonce = 2, to = 1, amount=10},
	     #cc{acc1 = 0, nonce = 3, acc2 = 1, bal1 = 10000, consensus_flag = true, id = 0, fee = 0}
	    ]),
    SignedParent = block_finality:top_block(),
    PHash = hash:doit(SignedParent#signed.data),
    Block = #block{txs = Txs, hash = PHash, height = 1},
    SignedBlock = keys:sign(Block),
    Txs2 = sign_all(
	     [#spend{from = 0, nonce = 4, to = 1, amount=10}
	     ]),
    PHash2 = hash:doit(SignedBlock#signed.data),
    Block2 = #block{txs = Txs2, hash = PHash2, height = 2},
    SignedBlock2 = keys:sign(Block2),
    absorb([SignedBlock, SignedBlock2]).
