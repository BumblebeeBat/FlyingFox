-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,read_int/2,account/1,account/2,account/3,channel/2,channel/3,absorb/1,is_key/1,height/1,height/0,txs/1]).
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = []}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {block = 0, number = 0, parent = finality, accounts = dict:new(), channels = dict:new()}).
-record(channel_close, {acc = 0, nonce = 0, id = 0}).
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
handle_call({read_int, Height, ParentKey}, _From, D) -> 
    {reply, read_int_internal(Height, ParentKey, D), D};
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
txs(X) -> X#x.block#signed.data#block.txs.
height() -> height(read(top)).
height(K) -> 
    X = read(K),
    X#x.number.
read_int(Height, BlockPointer) ->
    gen_server:call(?MODULE, {read_int, Height, BlockPointer}).
read_int_internal(Height, BlockPointer, D) ->
    BlockX = dict:fetch(BlockPointer, D),
    F = constants:finality(),
    if
	BlockX == finality -> block_finality:read(Height);
	BlockX#x.number - Height > F -> block_finality:read(Height);
	BlockX#x.number == Height -> BlockX;
	true -> read_int_internal(Height, BlockX#x.parent, D)
    end.
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
%check that it was validated by enough signers,
%check that the amount bonded is sufficiently big compared to the amount being spent, and the size of the block.
    Size = size(packer:pack(Block)),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    {ChannelsDict, AccountsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), BlockGap),
%give out rewards for validators in the digest.
%take fee from block creator in the digest.

    Key = hash:doit(SignedBlock#signed.data),
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, number = Parent#x.number + 1},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    gen_server:cast(?MODULE, {write, Key, V}).
    %look in AccountsDict to see if any new accounts use my pubkey. If they do, add them to id module.
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
%-record(acc, {balance = 0, nonce = 0, pub = 0}).
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
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1}).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
buy_block() -> buy_block(tx_pool:txs()).
buy_block(Txs) -> buy_block(Txs, 1).
buy_block(Txs, BlockGap) ->
    ParentX = read(read(top)),
    Parent = ParentX#x.block#signed.data,
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    Block = #block{txs = Txs, hash = PHash, number = N},
    absorb([keys:sign(Block)]),
    tx_pool:dump().
create_account(Pub, Amount) ->
    Id = keys:id(),
    Acc = account(Id),
    Tx = #ca{from = Id, nonce = Acc#acc.nonce + 1, pub = Pub, amount = Amount},
    tx_pool:absorb(keys:sign(Tx)).
spend(To, Amount) ->
    Id = keys:id(),
    Acc = account(Id),
    Tx = #spend{from = Id, nonce = Acc#acc.nonce + 1, to = To, amount =Amount},
    tx_pool:absorb(keys:sign(Tx)).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = account(Id),
    ChannelPointer = channel(ChannelId),%[-6,"channel",2,0,3]
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, read(top), ChannelId),
    TC = SignedToChannel#signed.data,
    SignedTx = keys:sign(#tc{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, bal1 = TC#tc.bal1 + Inc1, bal2 = TC#tc.bal2 + Inc2, consensus_flag = true, id = ChannelId, fee = Fee, nonce = Acc#acc.nonce + 1}),
    #signed{data = SignedTx#signed.data, sig2 = SignedTx#signed.sig2, sig = SignedTx#signed.sig, revealed = ChannelId}.

create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When first creating a new channel, don't add the id. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = account(Id),
    ToAcc = account(To),
    true = Acc#acc.balance > MyBalance,
    true = ToAcc#acc.balance > TheirBalance,
    Tx = #tc{acc1 = Id, acc2 = To, nonce = Acc#acc.nonce + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee},
    keys:sign(Tx).
timeout_channel(ChannelTx) ->
    Id = keys:id(),
    Acc = account(Id),
    Tx = #timeout{acc = Id, nonce = Acc#acc.nonce + 1, channel_block = keys:sign(ChannelTx)},
    tx_pool:absorb(keys:sign(Tx)).
close_channel(Id, Amount, Nonce) ->
    ChannelPointer = channel(Id),
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, read(top), Id),
    TC = SignedToChannel#signed.data,
    keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = true}).
channel_block(Id, Amount, Nonce, Delay) ->
    ChannelPointer = channel(Id),
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, read(top), Id),
    TC = SignedToChannel#signed.data,
    keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = false, delay = Delay}).
slow_close(Id) ->
    %ChannelPointer = channel(Id),
    %SignedTimeoutChannel = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, read(top), Id),
    %Timeout = SignedTimeoutChannel#signed.data,
    MyId = keys:id(),
    Acc = account(MyId),
    tx_pool:absorb(keys:sign(#channel_close{acc = MyId, nonce = Acc#acc.nonce + 1, id = Id})).
channel_slash(ChannelTx) ->
    MyId = keys:id(),
    Acc = account(MyId),
    tx_pool:absorb(keys:sign(#channel_slash{acc = MyId, nonce = Acc#acc.nonce, channel_block = ChannelTx})).
    
test() -> 
    {Pub, Priv} = sign:new_key(),
    create_account(Pub, 10000),
    spend(1, 10),
    buy_block(),
    CreateTx1 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx1 = sign:sign_tx(CreateTx1, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx1),
    CreateTx2 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx2 = sign:sign_tx(CreateTx2, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx3 = sign:sign_tx(CreateTx3, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx3),
    buy_block(),
    ToChannel = to_channel(0, 0, 10, 0),
    SignedToChannel = sign:sign_tx(ToChannel, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedToChannel),
    buy_block(),
    ChannelTx = close_channel(0, -200, 1),
    TimeoutTx = channel_block(1, -200, 1, 0),
    SlasherTx = channel_block(2, -200, 1, 10),
    SignedChannelTx = sign:sign_tx(ChannelTx, Pub, Priv, dict:new()),
    SignedTimeoutTx = sign:sign_tx(TimeoutTx, Pub, Priv, dict:new()),
    SignedSlasherTx = sign:sign_tx(SlasherTx, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedChannelTx),
    timeout_channel(SignedTimeoutTx),
    timeout_channel(SignedSlasherTx),
    buy_block(),
    SlashBlock = channel_block(2, 0, 2, 5),
    SignedSlashBlock = sign:sign_tx(SlashBlock, Pub, Priv,dict:new()),
    slow_close(1),
    channel_slash(SignedSlashBlock),
    buy_block(),
    success.
