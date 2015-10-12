-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,read_int/2,read_int/1,account/1,account/2,account/3,channel/2,channel/3,channel/1,absorb/1,is_key/1,height/1,height/0,txs/1,txs/0,power/0,power/1,block/0,block/1,buy_block/1, block_power/1,block_entropy/1,empty_block/0]).
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = [], power = 1, entropy = 0, total_coins = 0}).
%We need each block to say how much money is left.
block_power(B) -> B#block.power.
block_entropy(B) -> B#block.entropy.
empty_block() -> #block{}.
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {block = 0, height = 0, parent = finality, accounts = dict:new(), channels = dict:new()}).
init(ok) -> 
    SignedBlock = block_finality:top_block(),
    X = #x{block = SignedBlock},
    BH = hash:doit(SignedBlock#signed.data),
    D = dict:store(top, BH, dict:new()),
    E = dict:store(BH, X, D),
    {ok, E}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
read_int_internal(Height, BlockPointer, D) ->
    BlockX = dict:fetch(BlockPointer, D),
    F = constants:finality(),
    if
        BlockX == finality -> block_finality:read(Height);
        BlockX#x.height - Height > F -> block_finality:read(Height);
        BlockX#x.height == Height -> BlockX;
        true -> read_int_internal(Height, BlockX#x.parent, D)
    end.
handle_call({read_int, Height, ParentKey}, _From, D) -> 
    {reply, read_int_internal(Height, ParentKey, D), D};
handle_call(top, _From, D) -> 
    {reply, dict:fetch(dict:fetch(top, D), D), D};
handle_call({key, X}, _From, D) -> {reply, dict:is_key(X, D), D};
%handle_call({read, finality}, _From, D) -> {reply, block_finality:top(), D};%shouldn't just be top...
handle_call({read, V}, _From, D) -> 
    %read and read_int should be different calls. read_int needs to say which parent it came from.
    X = case dict:find(V, D) of
	    {ok, Value} -> Value;
	    error -> "none"
	end,
    {reply, X, D}.
handle_cast({write, K, V}, D) -> 
    Top = dict:fetch(dict:fetch(top, D), D),
    TopHeight = Top#x.block#signed.data#block.number,
    NewHeight = V#x.block#signed.data#block.number,
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
block() -> block(read(top)).
block(X) -> 
    Y = read(X),
    Y#x.block#signed.data.
txs() -> txs(read(read(top))).
txs(X) -> 
    X#x.block#signed.data#block.txs.
power() -> power(read(top)).
power(K) -> 
    X = read(K),
    X#x.block#signed.data#block.power.
height() -> height(read(top)).
height(K) -> 
    X = read(K),
    X#x.height.
read_int(Height) -> read_int(Height, read(top)).
read_int(Height, BlockPointer) ->
    gen_server:call(?MODULE, {read_int, Height, BlockPointer}).
    
write(SignedBlock) ->
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = is_key(BH),
    ParentKey = Block#block.hash,
    Parent = read(ParentKey),
    OldNumber = Parent#x.block#signed.data#block.number,%we need to charge more if this skipped height. 
    NewNumber = Block#block.number,
    BlockGap = NewNumber - OldNumber,
    true = BlockGap > 0,
%check that it has sign txs that validate it's parent block.
%each sign tx may have won multiple times.
    Winners = sign_tx:winners(Block#block.txs),
    true = Winners > (constants:minimum_validators_per_block() - 1),
%check that the amount bonded is within a small margin of the average of the last several blocks. Check that the amount being spent is less than 1/2 the amount bonded.
    Size = size(zlib:compress(term_to_binary(Block))),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    Entropy = entropy:doit(NewNumber),
    Entropy = Block#block.entropy, 
    {ChannelsDict, AccountsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), NewNumber),
%take fee from block creator in the digest.
    TcIncreases = to_channel_tx:tc_increases(NewNumber),
    CCLosses = channel_block_tx:cc_losses(Block#block.txs),
    NewPower = Parent#x.block#signed.data#block.power + TcIncreases - CCLosses,%increases from to_channel tx fed into finality (when the channel is still open) - decreases from channel closures in this block (for channels that have been open since finality).
    NewPower = SignedBlock#signed.data#block.power,
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, height = Parent#x.height + 1},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    Key = hash:doit(SignedBlock#signed.data),
    gen_server:cast(?MODULE, {write, Key, V}),
    tx_pool:dump().
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
account(N) -> account(N, tx_pool:accounts()).
account(N, AccountsDict) -> account(N, read(top), AccountsDict).
account(N, H, AccountsDict) ->
    B = dict:is_key(N, AccountsDict),
    if
        B -> dict:fetch(N, AccountsDict);
        true -> account_helper(N, H)
    end.
account_helper(N, finality) -> accounts:read_account(N);
account_helper(N, H) ->
    X = read(H),
    Accounts = X#x.accounts,
    Parent = X#x.parent,
    case dict:find(N, Accounts) of
        error -> account_helper(N, Parent);
        {ok, Val} -> Val
    end.
channel(N) -> channel(N, tx_pool:channels()).
channel(N, Channels) -> channel(N, read(top), Channels).
channel(N, H, Channels) ->
    B = dict:is_key(N, Channels),
    if
        B -> dict:fetch(N, Channels);
        true -> channel_helper(N, H)
    end.
channel_helper(N, finality) -> channels:read_channel(N);
channel_helper(N, H) ->
    X = read(H),
    Channels = X#x.channels,
    Parent = X#x.parent,
    case dict:find(N, Channels) of
        error -> channel_helper(N, Parent);
        {ok, Val} -> Val
    end.
buy_block() -> buy_block(tx_pool:txs()).
buy_block(Txs) -> buy_block(Txs, 1).
buy_block(Txs, BlockGap) ->
    ParentKey = read(top),
    ParentX = read(ParentKey),
    Parent = ParentX#x.block#signed.data,
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    TcIncreases = to_channel_tx:tc_increases(N),
    CCLosses = channel_block_tx:cc_losses(Txs),
    P = Parent#block.power + TcIncreases - CCLosses,
    Entropy = entropy:doit(N),
    Block = #block{txs = Txs, hash = PHash, number = N, power = P, entropy = Entropy},
    absorb([keys:sign(Block)]),
    tx_pool:dump().
sign_tx(Tx, Pub, Priv) -> sign:sign_tx(Tx, Pub, Priv, tx_pool:accounts()).
test() -> 
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 50000),
    spend_tx:spend(1, 10),
    sign_tx:sign(),
    buy_block(),
    Top = read(read(top)),
    1 = Top#x.block#signed.data#block.power,
    CreateTx1 = to_channel_tx:create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    tx_pool:absorb(SignedCreateTx1),
    CreateTx2 = to_channel_tx:create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = to_channel_tx:create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    tx_pool:absorb(SignedCreateTx3),
    sign_tx:sign(),
    buy_block(),
    Top2 = read(read(top)),
    1 = Top2#x.block#signed.data#block.power,
    ToChannel = to_channel_tx:to_channel(0, 0, 10, 0),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool:absorb(SignedToChannel),
    sign_tx:sign(),
    buy_block(),
    Top3 = read(read(top)),
    1 = Top3#x.block#signed.data#block.power,
    ChannelTx = channel_block_tx:close_channel(0, -200, 1),
    TimeoutTx = channel_block_tx:channel_block(1, -200, 1, 0),
    SlasherTx = channel_block_tx:channel_block(2, -200, 1, 10),
    SignedChannelTx = sign_tx(ChannelTx, Pub, Priv),
    SignedTimeoutTx = sign_tx(TimeoutTx, Pub, Priv),
    SignedSlasherTx = sign_tx(SlasherTx, Pub, Priv),
    Acc1 = account(1),
    A1 = accounts:balance(Acc1),
    tx_pool:absorb(SignedChannelTx),
    Acc2 = account(1),
    A2 = accounts:balance(Acc2),
    true = A2 < A1,%4000 fee per block, only gain 1010.
    channel_timeout_tx:timeout_channel(SignedTimeoutTx),
    channel_timeout_tx:timeout_channel(SignedSlasherTx),
    sign_tx:sign(),
    buy_block(),
    Top4 = read(read(top)),
    1 = Top4#x.block#signed.data#block.power,
    SlashBlock = channel_block_tx:channel_block(2, 0, 2, 5),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    channel_close_tx:slow_close(1),
    channel_slash_tx:channel_slash(SignedSlashBlock),
    sign_tx:sign(),
    buy_block(),
    Top5 = read(read(top)),
    1 = Top5#x.block#signed.data#block.power,
    success.
