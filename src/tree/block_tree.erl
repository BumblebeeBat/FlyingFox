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


sign_all([]) -> [];
sign_all([Tx|Txs]) -> [keys:sign(Tx)|sign_all(Txs)].
buy_block() -> buy_block(tx_pool:txs()).
buy_block(Txs) -> buy_block(Txs, 1).
buy_block(Txs, BlockGap) ->
    ParentX = read(read(top)),
    Parent = ParentX#x.block#signed.data,
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    Block = #block{txs = Txs, hash = PHash, number = N},
    absorb([keys:sign(Block)]).
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
    ChannelPointer = channel(ChannelId),
    #tc{acc1 = 0, acc2 = 1, bal1 = 10000, bal2 = 1010, consensus_flag = true, id = 0, fee = Fee}.
create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When first creating a new channel, don't add the id. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = account(Id),
    ToAcc = account(To),
    Tx = #tc{acc1 = Id, acc2 = To, nonce = Acc#acc.nonce + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee},
    keys:sign(Tx).
test() -> 
    {Pub, Priv} = sign:new_key(),
    create_account(Pub, 10000),
    spend(1, 10),
    SignedParent = block_finality:top_block(),
    SP = read_int(0, read(top)),
    SignedParent = SP#x.block,
    SP = read(hash:doit(SignedParent#signed.data)),
    buy_block(),
    tx_pool:dump(),
    SB = read_int(1, read(top)),
    SignedBlock = SB#x.block,
    SB = read(hash:doit(SignedBlock#signed.data)),
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
    SB = read_int(1, read(top)),
    SB2 = read_int(2, read(top)),
    SignedBlock2 = SB2#x.block,
    SB2 = read(hash:doit(SignedBlock2#signed.data)),

    CreateTxb = #tc{acc1 = 0, acc2 = 1, nonce = 6, bal1 = 10000, bal2 = 1010, consensus_flag = true, id = 0, fee = 0},
    SignedCreateTxb = sign:sign_tx(CreateTxb, Pub, Priv, dict:new()),
    SCTRb = #signed{data = SignedCreateTxb#signed.data, sig2 = SignedCreateTxb#signed.sig2, revealed = 0},
    Txs3 = sign_all(
	     [
	      SCTRb
	     ]),
    buy_block(Txs3),
    ChannelTx = #channel_block{acc1 = 0, acc2 = 1, amount = -200, nonce = 1, id = 0, fast = true},
    TimeoutTx = #channel_block{acc1 = 0, acc2 = 1, amount = -200, nonce = 1, id = 1, fast = false, delay = 0},
    SlasherTx = #channel_block{acc1 = 0, acc2 = 1, amount = -200, nonce = 1, id = 2, fast = false},%slash/timeout names flipped.
    SignedChannelTx = sign:sign_tx(ChannelTx, Pub, Priv, dict:new()),
    SignedTimeoutTx = sign:sign_tx(TimeoutTx, Pub, Priv, dict:new()),
    SignedSlasherTx = sign:sign_tx(SlasherTx, Pub, Priv, dict:new()),
    DSignedTimeoutTx = keys:sign(SignedTimeoutTx),
    DSignedSlasherTx = keys:sign(SignedSlasherTx),
    %These nonces are no good...
    Timeout = #timeout{acc = 0, nonce = 7, channel_block = DSignedTimeoutTx},
    Timeout2 = #timeout{acc = 0, nonce = 8, channel_block = DSignedSlasherTx},
    Txs4 = sign_all(
	     [SignedChannelTx,
	      Timeout,
	      Timeout2
	     ]),
    buy_block(Txs4),
    SlashBlock = #channel_block{acc1 = 0, acc2 = 1, amount = 0, nonce = 2, id = 2, fast = true},
    SignedSlashBlock = sign:sign_tx(SlashBlock, Pub, Priv,dict:new()),
    DSignedSlashBlock = keys:sign(SignedSlashBlock),
    CloseChannel = #channel_close{acc = 0, nonce = 9, id = 1},
    SlashChannel = #channel_slash{acc = 0, nonce = 10, channel_block = DSignedSlashBlock},
    Txs5 = sign_all(
	     [CloseChannel,
	      SlashChannel]),
    buy_block(Txs5),
    success.
