-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, long_test/0,test/0,write/1,unsafe_write/2,top/0,read/1,read_int/2,read_int/1,secret/4,account/1,account/2,account/3,channel/2,channel/3,channel/1,absorb/1,is_key/1,height/1,height/0,txs/1,txs/0,power/0,power/1,block/0,block/1,buy_block/2, block_power/1,block_entropy/1,empty_block/0,total_coins/0, buy_block/0, block_number/1, block2txs/1, block_root/1,backup/1,x_to_block/1,check/0]).
-record(block, {acc = 0, number = 0, hash = "", txs = [], power = fractions:multiply_int(constants:initial_portion_delegated(), constants:initial_coins()), entropy = 0, total_coins = constants:initial_coins(), db_root = <<>>}).
%power is how many coin are in channels. it is for consensus.
block_root(B) -> B#block.db_root.
block_number(B) -> B#block.number.
block_power(B) -> B#block.power.
block_entropy(B) -> B#block.entropy.
empty_block() -> #block{}.
-record(x, {block = 0, height = 0, parent = finality, accounts = dict:new(), channels = dict:new(), secrets = dict:new()}).%height always increases by 1. 
x_to_block(X) -> X#x.block.
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
        BlockX#x.height == Height -> BlockX#x.block;
        true -> read_int_internal(Height, BlockX#x.parent, D)
    end.
handle_cast(_, X) -> {noreply, X}.
handle_call({read_int, Height, ParentKey}, _From, D) -> 
    {reply, read_int_internal(Height, ParentKey, D), D};
handle_call(top, _From, D) -> %this is bad, just use read.
    {reply, dict:fetch(dict:fetch(top, D), D), D};
handle_call({key, X}, _From, D) -> {reply, dict:is_key(X, D), D};
handle_call({read, V}, _From, D) -> 
    X = case dict:find(V, D) of
	    {ok, Value} -> Value;
	    error -> <<"none">>
	end,
    {reply, X, D};
handle_call({unsafe_write, K, V}, _From, D) -> 
    NewBlock = sign:data(V#x.block),
    ND = dict:store(top, hash:doit(NewBlock), D),
    {reply, 0, dict:store(K, V, ND)};
handle_call(check, _From, D) -> {reply, D, D};
handle_call({write, K, V}, _From, D) -> 
    T = dict:fetch(top, D),
    Top = dict:fetch(T, D),
    NewBlock = sign:data(V#x.block),
    TopHeightB = sign:data(Top#x.block),
    TopHeight = TopHeightB#block.number,
    NewHeight = NewBlock#block.number,
    Finality = constants:finality(),
    L = length(dict:fetch_keys(D)),
    ND = if
	     NewHeight > TopHeight ->
	    %possible pruning, and merge digests into finality.
		 DD = if 
			  L > Finality -> 
			  %NewHeight > Finality ->
			      {Child, OldKey, X} = merger(T, D),
			      Block = sign:data(X#x.block),
			      BN = block_tree:block_number(Block),
			      A = X#x.accounts,
			      C = X#x.channels,
			      S = X#x.secrets,
			      Z = backup(NewHeight),
			      if
				  Z ->
				      H = backup:hash(),
				      H = NewBlock#block.db_root,
				      backup:backup(),
				      0;
				  true -> 0
			      end,
			      if BN > 0 -> block_finality:append(X#x.block, X#x.height); true -> 0 end,
			      finality_absorb(S, A, C),
			      P = dict:fetch(Child, D),
			      NewP = #x{block = P#x.block, height = P#x.height, parent = finality, accounts = P#x.accounts, channels = P#x.channels, secrets = P#x.secrets},
			      dict:store(Child, NewP, dict:erase(OldKey, D));
		     true  -> D
		 end,
                 txs:dump(),
                 dict:store(top, hash:doit(NewBlock), DD);
        true -> D
    end,
    {reply, 0, dict:store(K, V, ND)}.
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
	finality -> {<<"none">>, Key, X};
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
total_coins() -> total_coins(sign:data(block(read(read(top))))).
total_coins(X) -> 
    X#block.total_coins.
block() -> block(read(read(top))).
block(X) when is_record(X, x) -> 
    X#x.block;
block(<<"none">>) -> 
    1=2;
block(X) -> 
    block(read(X)).
%if
	%is_record(X, x) -> sign:data(X#x.block);
%is_record(X, x) -> X#x.block;
%true -> block(read(X))
%end.
block2txs(X) -> X#block.txs.
txs() -> txs(read(read(top))).
txs(X) -> 
    B = sign:data(X),
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
unsafe_write(SignedBlock, ParentKey) ->
    Block = sign:data(SignedBlock),
    NewNumber = Block#block.number,
    Winners = sign_tx:winners(Block#block.txs),
    true = Winners > (constants:minimum_validators_per_block() - 1),
%check that the amount bonded is within a small margin of the average of the last several blocks. Check that the amount being spent is less than 1/2 the amount bonded.
    %Size = size(zlib:compress(term_to_binary(Block))),
    %true = Size < constants:max_block_size(),
    %Entropy = entropy:doit(NewNumber),
    %Entropy = Block#block.entropy, 
    %NewTotalCoins = Block#block.total_coins,
    %{ChannelsDict, AccountsDict, _, Secrets} = txs:digest(Block#block.txs, finality, dict:new(), dict:new(), NewTotalCoins, dict:new(), NewNumber),
    V = #x{accounts = dict:new(), channels = dict:new(), block = SignedBlock, parent = ParentKey, height = NewNumber, secrets = dict:new()},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    Key = hash:doit(sign:data(SignedBlock)),
    gen_server:call(?MODULE, {unsafe_write, Key, V}),
    tx_pool:dump(Block#block.total_coins),
    Key.
    
write(SignedBlock) ->
    %PSB = packer:pack(SignedBlock),
    %SignedBlock = packer:unpack(PSB),
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
    true = Size < constants:max_block_size(),
    Entropy = entropy:doit(NewNumber),
    Entropy = Block#block.entropy, 
    io:fwrite("right here"),
    {ChannelsDict, AccountsDict, NewTotalCoins, Secrets} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), Parent#block.total_coins, dict:new(), NewNumber),
    NewTotalCoins = Block#block.total_coins,
%take fee from block creator in the digest.
    %TCIncreases and CCLosses this way is no good.
    %Instead, look at tc increases in the most recent block, and cc_losses in the most recent block. The estimate is less precise, but more accurate. The estimate has a bigger bell curve, but at least the bell curve's center can't be adjusted by an adversary. 
    TcIncreases = to_channel_tx:tc_increases(Block#block.txs),
    CCLosses = channel_block_tx:cc_losses(Block#block.txs),
    RepoLosses = repo_tx:losses(Block#block.txs),
    CFLLosses = channel_funds_limit_tx:losses(Block#block.txs, dict:new(), ParentKey),
    NewPower = power(Parentx#x.block) + TcIncreases - CCLosses - RepoLosses - CFLLosses,%increases from to_channel tx fed into finality (when the channel is still open) - decreases from channel closures in this block (for channels that have been open since finality).
    NewPower = power(SignedBlock),
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, height = Parentx#x.height + 1, secrets = Secrets},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    Key = hash:doit(sign:data(SignedBlock)),
    gen_server:call(?MODULE, {write, Key, V}),
    tx_pool:dump(Block#block.total_coins).
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
%secret(N, SH) -> secret(N, SH, tx_pool:secrets()).
%secret(N, SH, SecretsDict) -> secret(N, SH, read(top), SecretsDict).
check() ->
    gen_server:call(?MODULE, check).
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
    S = size(X),
    if
	S == 4 ->
	    io:fwrite("N is "),
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n"),
	    io:fwrite(X);
	true -> 0
    end,
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
backup(N) ->
   (N rem (fractions:multiply_int(constants:backup(), constants:max_reveal()))) == 0.
buy_block() -> buy_block(tx_pool:txs(), tx_pool:total_coins()).
buy_block(Txs, TotalCoins) -> buy_block(Txs, TotalCoins, 1).
buy_block(Txs, TotalCoins, BlockGap) ->
    ParentKey = read(top),
    ParentX = read(ParentKey),
    Parent = sign:data(ParentX#x.block),
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    TcIncreases = to_channel_tx:tc_increases(Txs),
    CCLosses = channel_block_tx:cc_losses(Txs),
    RepoLosses = repo_tx:losses(Txs),
    CFLLosses = channel_funds_limit_tx:losses(Txs, tx_pool:channels(), read(top)),
    P = Parent#block.power + TcIncreases - CCLosses - RepoLosses - CFLLosses,
    Entropy = entropy:doit(N),
    Z = backup(N),
    %Z = N rem constants:finality(),
    DBR = if
	Z  -> backup:hash();
	true -> <<>>
    end,
    Block = #block{txs = Txs, hash = PHash, number = N, power = P, entropy = Entropy, total_coins = TotalCoins, db_root = DBR},
    absorb([keys:sign(Block)]).
sign_tx(Tx, Pub, Priv) -> sign:sign_tx(Tx, Pub, Priv, tx_pool:accounts()).
test() -> 
    io:fwrite("block tree test 0"),
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 620000, 0),
    spend_tx:spend(1, 10, 0),
    sign_tx:sign(),
    reveal:reveal(),
    io:fwrite("block tree test 1"),
    buy_block(),
    A3 = sign_tx(slasher_tx:slasher(1, keys:sign({sign_tx, 0, 0, 0, 0, 0, 0})), Pub, Priv),
    io:fwrite("block tree test 103"),
    tx_pool:absorb(A3),
    io:fwrite("block tree test 11"),
    CreateTx1 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    io:fwrite("block tree test 12"),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    io:fwrite("block tree test 13"),
    tx_pool:absorb(SignedCreateTx1),
    io:fwrite("block tree test 2"),
    CreateTx2 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    tx_pool:absorb(SignedCreateTx3),
    sign_tx:sign(),
    io:fwrite("block tree test 3"),
    reveal:reveal(),
    buy_block(),
    ToChannel = to_channel_tx:to_channel(24000, 0, 10, 0),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool:absorb(SignedToChannel),
    sign_tx:sign(),
    io:fwrite("block tree test 4"),
    reveal:reveal(),
    buy_block(),%needs to start with some big channels with myself, so I have enough delegation.
    ChannelTx = channel_block_tx:close_channel(24000, -200, 1, 0),
    TimeoutTx = channel_block_tx:channel_block(24001, -200, 1, 0, 0),
    SlasherTx = channel_block_tx:channel_block(24002, -200, 1, 10, 0),
    SignedChannelTx = sign_tx(ChannelTx, Pub, Priv),
    SignedTimeoutTx = sign_tx(TimeoutTx, Pub, Priv),
    SignedSlasherTx = sign_tx(SlasherTx, Pub, Priv),
    Acc1 = account(1),
    A1 = accounts:balance(Acc1),
    channel_block_tx:publish_channel_block(SignedChannelTx, 0, []),
    io:fwrite("block tree test 5"),
    %tx_pool:absorb(SignedChannelTx),
    Acc2 = account(1),
    A2 = accounts:balance(Acc2),
    true = A2 < A1,%4000 fee per block, only gain 1010.
    channel_timeout_tx:timeout_channel(SignedTimeoutTx, []),
    channel_timeout_tx:timeout_channel(SignedSlasherTx, []),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    channel_close_tx:slow_close(24001),
    SlashBlock = channel_block_tx:channel_block(24002, 0, 2, 5, 0),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    AccOne = account(1),
    ChannelSlashTx = channel_slash_tx:make_tx(1, SignedSlashBlock, 0),
    %ChannelSlashTx = {channel_slash, 1, accounts:nonce(AccOne), SignedSlashBlock},
    SignedChannelSlashTx = sign:sign_tx(ChannelSlashTx, Pub, Priv, tx_pool:accounts()),
    tx_pool:absorb(SignedChannelSlashTx),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    sign_tx:sign(),
    reveal:reveal(),
    {Pub2, Priv2} = sign:new_key(),
    create_account_tx:create_account(Pub2, 650000, 0),
    buy_block(),
    sign_tx:sign(),
    reveal:reveal(),
    tx_pool:absorb(sign_tx(delete_account_tx:delete_account(2, 0, 0), Pub2, Priv2)),
    create_account_tx:create_account(Pub2, 680000, 0),
    buy_block(),
    CreateTxq = to_channel_tx:create_channel(2, 110002, 0, <<"delegated_2">>, 0),
    SignedCreateTxq = sign_tx(CreateTxq, Pub2, Priv2),
    EmptyChannel = channels:empty(),
    true = EmptyChannel == channel(24000),
    tx_pool:absorb(SignedCreateTxq),
    false = EmptyChannel == channel(24000),
    reveal:reveal(),
    sign_tx:sign(),
    success.
long_test() -> 
    {Pub, Priv} = sign:new_key(),
    create_account_tx:create_account(Pub, 620000, 0),
    spend_tx:spend(1, 10, 0),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    tx_pool:absorb(sign_tx(slasher_tx:slasher(1, keys:sign({sign_tx, 0, 0, 0, 0, 0, 0})), Pub, Priv)),
    Top = read(read(top)),
    CreateTx1 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    %channel_block_tx:publish_channel_block(SignedCreateTx1, 0, []),
    tx_pool:absorb(SignedCreateTx1),
    CreateTx2 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    %channel_block_tx:publish_channel_block(SignedCreateTx2, 0, []),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    %channel_block_tx:publish_channel_block(SignedCreateTx3, 0, []),
    tx_pool:absorb(SignedCreateTx3),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top2 = read(read(top)),
    ToChannel = to_channel_tx:to_channel(24000, 0, 10, 0),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool:absorb(SignedToChannel),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),%needs to start with some big channels with myself, so I have enough delegation.
    Top3 = read(read(top)),
    ChannelTx = channel_block_tx:close_channel(24000, -200, 1, 0),
    TimeoutTx = channel_block_tx:channel_block(24001, -200, 1, 0, 0),
    SlasherTx = channel_block_tx:channel_block(24002, -200, 1, 10, 0),
    SignedChannelTx = sign_tx(ChannelTx, Pub, Priv),
    SignedTimeoutTx = sign_tx(TimeoutTx, Pub, Priv),
    SignedSlasherTx = sign_tx(SlasherTx, Pub, Priv),
    Acc1 = account(1),
    A1 = accounts:balance(Acc1),
    channel_block_tx:publish_channel_block(SignedChannelTx, 0, []),
    %tx_pool:absorb(SignedChannelTx),
    Acc2 = account(1),
    A2 = accounts:balance(Acc2),
    true = A2 < A1,%4000 fee per block, only gain 1010.
    channel_timeout_tx:timeout_channel(SignedTimeoutTx, []),
    channel_timeout_tx:timeout_channel(SignedSlasherTx, []),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    Top4 = read(read(top)),
    %1 = power(Top4#x.block),
    channel_close_tx:slow_close(24001),
    SlashBlock = channel_block_tx:channel_block(24002, 0, 2, 5, 0),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    AccOne = account(1),
    ChannelSlashTx = channel_slash_tx:make_tx(1, SignedSlashBlock, 0),
    %ChannelSlashTx = {channel_slash, 1, accounts:nonce(AccOne), SignedSlashBlock},
    SignedChannelSlashTx = sign:sign_tx(ChannelSlashTx, Pub, Priv, tx_pool:accounts()),
    tx_pool:absorb(SignedChannelSlashTx),
    sign_tx:sign(),
    reveal:reveal(),
    buy_block(),
    sign_tx:sign(),
    reveal:reveal(),
    Top5 = read(read(top)),
    {Pub2, Priv2} = sign:new_key(),
    create_account_tx:create_account(Pub2, 650000, 0),
    buy_block(),
    sign_tx:sign(),
    reveal:reveal(),
    tx_pool:absorb(sign_tx(delete_account_tx:delete_account(2, 0, 0), Pub2, Priv2)),
    {Pub3, Priv3} = sign:new_key(),
    create_account_tx:create_account(Pub2, 680000, 0),
    buy_block(),
    CreateTxq = to_channel_tx:create_channel(2, 110000, 0, <<"delegated_2">>, 0),
    SignedCreateTxq = sign_tx(CreateTxq, Pub2, Priv2),
    EmptyChannel = channels:empty(),
    true = EmptyChannel == channel(24000),
    %channel_block_tx:publish_channel_block(SignedCreateTxq, 0, []),
    tx_pool:absorb(SignedCreateTxq),
    false = EmptyChannel == channel(24000),
    reveal:reveal(),
    sign_tx:sign(),

    F = fun() -> sign_tx:sign(), buy_block() end,
    G = fun() -> F(), F(), F(), F(), F(), F(), F(), F() end,
    H = fun() -> G(), G(), G(), G(), G(), G(), G(), G() end,
    H(),
    SHtestslashed = sign_tx:secret_hash(sign:data(hd(tl(tl(block_tree:block2txs(sign:data(block_finality:read(1)))))))),
    false = block_tree:secret(0, SHtestslashed, block_tree:read(top), dict:new()),
    SHtest = sign_tx:secret_hash(sign:data(hd(block_tree:block2txs(sign:data(block_finality:read(10)))))),
    true = block_tree:secret(9, SHtest, block_tree:read(top), dict:new()),
    true = all_secrets:exists(9, SHtest),%because block 10 contains signatures over block 9.
    D1a = accounts:delegated(account(0)),
    D2a = accounts:delegated(account(1)),
    %to_channel_tx:create_channel(1, 10000, 1000, non_delegated, 0),
    %to_channel_tx:create_channel(1, 10000, 1000, delegated_2, 0),
    F2 = fun() -> sign_tx:sign(), reveal:reveal(), buy_block() , timer:sleep(200) end,
    G2 = fun() -> F2(), F2(), F2(), F2(), F2(), F2(), F2(), F2() end,
    H2 = fun() -> G2(), G2(), G2(), G2(), G2(), G2(), G2(), G2() end,
    H2(),
    false = all_secrets:exists(9, SHtest),
    D1b = accounts:delegated(account(0)),
    D2b = accounts:delegated(account(1)),
    D2a = D2b,
    D1a = D1b,
    0 = accounts:delegated(block_tree:account(1)),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    %H2(),
    %H2(),
    %This next test only works if account 2 is low enough on money. If some constants were changed, then we may need to run H2() more times before the next step.
    EmptyAccount = accounts:empty(),
    Acc2a = account(2),
    false = EmptyAccount == Acc2a,
    channel_funds_limit_tx:make_tx(24000, 0),
    repo_tx:repo(2, 0, []),
    Acc2b = account(2),
    true = EmptyAccount == Acc2b,
    success.
