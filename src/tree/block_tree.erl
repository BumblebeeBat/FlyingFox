-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,read_int/2,read_int/1,secret/4,account/1,account/2,account/3,channel/2,channel/3,channel/1,absorb/1,is_key/1,height/1,height/0,txs/1,txs/0,power/0,power/1,block/0,block/1,buy_block/2, block_power/1,block_entropy/1,empty_block/0,total_coins/0, buy_block/0, block_number/1, block2txs/1]).
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = [], power = 1, entropy = 0, total_coins = constants:initial_coins()}).
%power is how many coin are in channels. it is for consensus.
block_number(B) -> B#block.number.
block_power(B) -> B#block.power.
block_entropy(B) -> B#block.entropy.
empty_block() -> #block{}.
-record(x, {block = 0, height = 0, parent = finality, accounts = dict:new(), channels = dict:new(), secrets = dict:new()}).%height always increases by 1. 

init(ok) -> 
    SignedBlock = block_finality:top_block(),
    X = #x{block = SignedBlock},
    BH = hash:doit(sign:data(SignedBlock)),
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
handle_call(top, _From, D) -> %this is bad, just use read.
    {reply, dict:fetch(dict:fetch(top, D), D), D};
handle_call({key, X}, _From, D) -> {reply, dict:is_key(X, D), D};
handle_call({read, V}, _From, D) -> 
    X = case dict:find(V, D) of
	    {ok, Value} -> Value;
	    error -> "none"
	end,
    {reply, X, D}.
handle_cast({write, K, V}, D) -> 
    T = dict:fetch(top, D),
    Top = dict:fetch(T, D),
    NewBlock = sign:data(V#x.block),
    TopHeightB = sign:data(Top#x.block),
    TopHeight = TopHeightB#block.number,
    NewHeight = NewBlock#block.number,
    Finality = constants:finality(),
    ND = if
        NewHeight > TopHeight -> 
                 %possible pruning, and merge digests into finality.
		 DD = if 
			  NewHeight > Finality ->
			      {Child, MK, M} = merger(T, D),
			      Block = sign:data(M#x.block),
			      BN = block_tree:block_number(Block),
			      if BN > 0 -> block_finality:append(M#x.block); true -> 0 end,
			      A = M#x.accounts,
			      C = M#x.channels,
			      S = M#x.secrets,
			      finality_absorb(S, A, C),
			      P = dict:fetch(Child, D),
			      NewP = #x{block = P#x.block, height = P#x.height, parent = finality, accounts = P#x.accounts, channels = P#x.channels, secrets = P#x.secrets},
			      dict:store(Child, NewP, dict:erase(MK, D));
		     true  -> D
		 end,
                 txs:dump(),
                 dict:store(top, hash:doit(NewBlock), DD);
        true -> D
    end,
    {noreply, dict:store(K, V, ND)}.
absorb_accounts([], _) -> ok;
absorb_accounts([K|Keys], Accounts) -> 
    accounts:write(K, dict:fetch(K, Accounts)),
    absorb_accounts(Keys, Accounts).
absorb_channels([], _) ->ok;
absorb_channels([Ch|Chs], Channels) ->
    channels:write(Ch, dict:fetch(Ch, Channels)),
    absorb_channels(Chs, Channels).
absorb_secrets([], _) -> ok;
absorb_secrets([K|Keys], Secrets) -> 
    B = dict:fetch(K, Secrets),
    {Height, SH} = K,
    if
	B -> all_secrets:add(Height, SH);
	true -> all_secrets:remove(Height, SH)
    end,
    absorb_secrets(Keys, Secrets).
finality_absorb(Secrets, Accounts, Channels) ->
    AK = dict:fetch_keys(Accounts),
    absorb_accounts(AK, Accounts),
    CK = dict:fetch_keys(Channels),
    absorb_channels(CK, Channels),
    SK = dict:fetch_keys(Secrets),
    absorb_secrets(SK, Secrets).

merger(Key, D) ->
    X = dict:fetch(Key, D),
    case X#x.parent of
	finality -> {none, Key, X};
	Y -> merger(Key, Y, D)
    end.
merger(Child, Key, D) ->
    X = dict:fetch(Key, D),
    case X#x.parent of
	finality -> {Child, Key, X};
	Y -> merger(Key, Y, D)
    end.
top() -> gen_server:call(?MODULE, top).
is_key(X) -> gen_server:call(?MODULE, {key, X}).
read(K) -> gen_server:call(?MODULE, {read, K}).
block() -> block(read(top)).
total_coins() -> total_coins(block(read(top))).
total_coins(X) -> 
    X#block.total_coins.
block(X) -> 
    Y = read(X),
    sign:data(Y#x.block).
block2txs(X) -> X#block.txs.
txs() -> txs(read(read(top))).
txs(X) -> 
    B = sign:data(X#x.block),
    B#block.txs.
power() -> power(read(read(top))).
power(X) -> 
    A = element(1, X),
    B = case A of
	x -> sign:data(X#x.block);
	signed -> sign:data(X);
	block -> X
    end,
    B#block.power.
height() -> height(read(top)).
height(K) -> 
    X = read(K),
    X#x.height.
read_int(Height) -> 
    true = Height > -1,
    true = Height < height() + 1,
    read_int(Height, read(top)).
read_int(Height, BlockPointer) ->
    true = Height > -1,
    gen_server:call(?MODULE, {read_int, Height, BlockPointer}).
    
write(SignedBlock) ->
    Block = sign:data(SignedBlock),
    BH = hash:doit(Block),
    false = is_key(BH),
    ParentKey = Block#block.hash,
    Parentx = read(ParentKey),
    Parent = sign:data(Parentx#x.block),%we need to charge more if this skipped height. 
    OldNumber = Parent#block.number,
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
    {ChannelsDict, AccountsDict, NewTotalCoins, Secrets} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), Parent#block.total_coins, dict:new(), NewNumber),
    NewTotalCoins = Block#block.total_coins,
%take fee from block creator in the digest.
    TcIncreases = to_channel_tx:tc_increases(NewNumber, ParentKey),
    CCLosses = channel_block_tx:cc_losses(Block#block.txs),
    NewPower = power(Parentx#x.block) + TcIncreases - CCLosses,%increases from to_channel tx fed into finality (when the channel is still open) - decreases from channel closures in this block (for channels that have been open since finality).
    NewPower = power(SignedBlock),
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, height = Parentx#x.height + 1, secrets = Secrets},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    Key = hash:doit(sign:data(SignedBlock)),
    gen_server:cast(?MODULE, {write, Key, V}),
    tx_pool:dump(Block#block.total_coins).
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
%secret(N, SH) -> secret(N, SH, tx_pool:secrets()).
%secret(N, SH, SecretsDict) -> secret(N, SH, read(top), SecretsDict).
secret(N, SH, H, SecretsDict) ->
    Key = {N, SH},
    B = dict:is_key(Key, SecretsDict),
    if
        B -> dict:fetch(Key, SecretsDict);
        true -> secret_helper(Key, H)
    end.
secret_helper({N, SH}, finality) -> all_secrets:exists(N, SH);
secret_helper(Key, H) -> 
    X = read(H),
    Secrets = X#x.secrets,
    Parent = X#x.parent,
    case dict:find(Key, Secrets) of
	error -> secret_helper(Key, Parent);
	{ok, Val} -> Val
    end.

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
buy_block() -> buy_block(tx_pool:txs(), tx_pool:total_coins()).
buy_block(Txs, TotalCoins) -> buy_block(Txs, TotalCoins, 1).
buy_block(Txs, TotalCoins, BlockGap) ->
    ParentKey = read(top),
    ParentX = read(ParentKey),
    Parent = sign:data(ParentX#x.block),
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    TcIncreases = to_channel_tx:tc_increases(N, ParentKey),
    CCLosses = channel_block_tx:cc_losses(Txs),
    P = Parent#block.power + TcIncreases - CCLosses,
    Entropy = entropy:doit(N),
    Block = #block{txs = Txs, hash = PHash, number = N, power = P, entropy = Entropy, total_coins = TotalCoins},
    absorb([keys:sign(Block)]).
sign_tx(Tx, Pub, Priv) -> sign:sign_tx(Tx, Pub, Priv, tx_pool:accounts()).
test() -> 
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 1500000),
    spend_tx:spend(1, 10),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top = read(read(top)),
    1 = power(Top#x.block),
    CreateTx1 = to_channel_tx:create_channel(1, 10000, 1000, delegated_1, 0),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    tx_pool:absorb(SignedCreateTx1),
    CreateTx2 = to_channel_tx:create_channel(1, 10000, 1000, delegated_1, 0),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = to_channel_tx:create_channel(1, 10000, 1000, delegated_1, 0),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    tx_pool:absorb(SignedCreateTx3),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top2 = read(read(top)),
    1 = power(Top2#x.block),
    ToChannel = to_channel_tx:to_channel(0, 0, 10, 0),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool:absorb(SignedToChannel),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top3 = read(read(top)),
    1 = power(Top3#x.block),
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
    reveal:reveal(),
    buy_block(),
    Top4 = read(read(top)),
    1 = power(Top4#x.block),
    channel_close_tx:slow_close(1),
    SlashBlock = channel_block_tx:channel_block(2, 0, 2, 5),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    AccOne = account(1),
    ChannelSlashTx = {channel_slash, 1, accounts:nonce(AccOne), SignedSlashBlock},
    SignedChannelSlashTx = sign:sign_tx(ChannelSlashTx, Pub, Priv, tx_pool:accounts()),
    tx_pool:absorb(SignedChannelSlashTx),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top5 = read(read(top)),
    1 = power(Top5#x.block),
    %F = fun() -> sign_tx:sign(), reveal:reveal(), buy_block() end,
    F = fun() -> sign_tx:sign(), buy_block() end,
    G = fun() -> F(), F(), F(), F(), F(), F(), F(), F() end,
    H = fun() -> G(), G(), G(), G(), G(), G(), G(), G() end,
    H(),
    SHtest = sign_tx:secret_hash(sign:data(hd(block_tree:block2txs(sign:data(block_finality:read(10)))))),
    true = all_secrets:exists(9, SHtest),%because block 10 contains signatures over block 9.
    D1a = accounts:delegated(account(0)),
    D2a = accounts:delegated(account(1)),
    %to_channel_tx:create_channel(1, 10000, 1000, non_delegated, 0),
    %to_channel_tx:create_channel(1, 10000, 1000, delegated_2, 0),
    F2 = fun() -> sign_tx:sign(), reveal:reveal(), buy_block() end,
    G2 = fun() -> F2(), F2(), F2(), F2(), F2(), F2(), F2(), F2() end,
    H2 = fun() -> G2(), G2(), G2(), G2(), G2(), G2(), G2(), G2() end,
    H2(),
    false = all_secrets:exists(10, SHtest),
    D1b = accounts:delegated(account(0)),
    D2b = accounts:delegated(account(1)),
    D2a = D2b,
    D1a = D1b,
    0 = accounts:delegated(block_tree:account(1)),
    success.
