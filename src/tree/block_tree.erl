-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/0,read/1,read_int/2,account/1,account/2,account/3,channel/2,channel/3,channel/1,absorb/1,is_key/1,height/1,height/0,txs/1,txs/0,power/0,power/1,block/0,block/1]).
-record(block, {acc = 0, number = 0, hash = "", bond_size = 5000000, txs = [], power = 1, entropy = 0}).
%mix 1 bit of entropy in for each signer, order bits: ones first, then zeros.
%mix about 256 div 26 bits into entropy from each block. 
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(x, {block = 0, height = 0, parent = finality, accounts = dict:new(), channels = dict:new()}).
-record(channel_close, {acc = 0, nonce = 0, id = 0}).
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
read_int(Height, BlockPointer) ->
    gen_server:call(?MODULE, {read_int, Height, BlockPointer}).
-record(sign_tx, {acc = 0, nonce = 0, secret_hash = [], winners = [], prev_hash = ""}).
winners(L) -> winners(L, 0).
winners([], A) -> A;
winners([#signed{data = Tx}|T], A) when is_record(Tx, sign_tx)-> 
    B = A + length(Tx#sign_tx.winners),
    winners(T, B);
winners([_|T], A) -> winners(T, A).
    
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
    Winners = winners(Block#block.txs),
    true = Winners > (constants:minimum_validators_per_block() - 1),
%check that the amount bonded is within a small margin of the average of the last several blocks. Check that the amount being spent is less than 1/2 the amount bonded.
    Size = size(zlib:compress(packer:pack(Block))),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    {ChannelsDict, AccountsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), BlockGap),
%give out rewards for validators in the digest.
%take fee from block creator in the digest.
    TcIncreases = tc_increases(NewNumber),
    CCLosses = cc_losses(Block#block.txs),
    NewPower = Parent#x.block#signed.data#block.power + TcIncreases - CCLosses,%increases from to_channel tx fed into finality (when the channel is still open) - decreases from channel closures in this block (for channels that have been open since finality).
    NewPower = SignedBlock#signed.data#block.power,
    V = #x{accounts = AccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, height = Parent#x.height + 1},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    Key = hash:doit(SignedBlock#signed.data),
    gen_server:cast(?MODULE, {write, Key, V}),
    tx_pool:dump().
    %look in AccountsDict to see if any new accounts use my pubkey. If they do, add them to id module.
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
cc_losses(Txs) -> cc_losses(Txs, 0).%filter out channel_block, channel_slash, and channel_close type txs. add up the amount of money in each such channel. Exclude channels that Haven't been open since finality.
cc_losses([], X) -> X;
cc_losses([#signed{data = Tx}|T], X) when is_record(Tx, channel_block) -> 
    ParentKey = read(top),
    ChannelPointer = channel(Tx#channel_block.id, dict:new()),
    SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    OriginTx = SignedOriginTx#signed.data,
    StartAmount = OriginTx#tc.bal1 + OriginTx#tc.bal2,

    FChannelPointer = channels:read_channel(Tx#channel_block.id),
    FSignedOriginTx = channel_block_tx:origin_tx(FChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    FOriginTx = FSignedOriginTx#signed.data,
    if
        (FOriginTx#tc.acc1 == OriginTx#tc.acc1) and
        (FOriginTx#tc.acc2 == OriginTx#tc.acc2) ->
            SA = StartAmount;
        true -> SA = 0
    end,
    cc_losses(T, X+SA);
cc_losses([#signed{data = Tx}|T], X) when is_record(Tx, channel_slash) -> 
    cc_losses([Tx#channel_slash.channel_block|T], X);
cc_losses([#signed{data = Tx}|T], X) when is_record(Tx, channel_close) -> 
    ParentKey = read(top),
    Id = Tx#channel_close.id,
    ChannelPointer = block_tree:channel(Id, dict:new()),
    SignedOriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    OriginTimeout = SignedOriginTimeout#signed.data,
    CB = OriginTimeout#timeout.channel_block,
    cc_losses([CB|T], X);
cc_losses([_|T], X) -> cc_losses(T, X).

tc_increases(NewNumber) ->
    ParentKey = read(top),
    CF = constants:finality(),
    if
        NewNumber < CF -> TcIncreases = 0;
        true -> 
            FBlock = read_int(NewNumber - CF, ParentKey),
            TcIncreases = tc_increases(FBlock#block.txs, 0)
    end,
    TcIncreases.
%filter out tc type txs. add up the amount of money in each channel. Exclude channels that aren't still open.
tc_increases([], X) -> X;
tc_increases([#signed{data = Tx}|T], X) when is_record(Tx, tc) ->
    A = Tx#tc.increment,
    Id = Tx#tc.id,
    ChannelPointer = block_tree:channel(Id, dict:new()),
    ParentKey = read(top),
    SignedOriginTx = channel_block_tx:origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    OriginTx = SignedOriginTx#signed.data,
    if
        (Tx#tc.acc1 == OriginTx#tc.acc1) and
        (Tx#tc.acc2 == OriginTx#tc.acc2) ->
            SA = A;
        true -> SA = 0
    end,
    
    tc_increases(T, X+SA).
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
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
buy_block() -> buy_block(tx_pool:txs()).
buy_block(Txs) -> buy_block(Txs, 1).
buy_block(Txs, BlockGap) ->
    ParentKey = read(top),
    ParentX = read(ParentKey),
    Parent = ParentX#x.block#signed.data,
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    TcIncreases = tc_increases(N),
    CCLosses = cc_losses(Txs),
    P = Parent#block.power + TcIncreases - CCLosses,
    Block = #block{txs = Txs, hash = PHash, number = N, power = P},
    absorb([keys:sign(Block)]),
    tx_pool:dump().
winners(MyPower, TotalPower, Entropy, Pubkey) ->
    winners(MyPower, TotalPower, Entropy, Pubkey, 0, constants:chances_per_address(), []).
winners(_, _, _, _, J, Limit, Out) when J > Limit -> Out;
winners(MyPower, TotalPower, Entropy, Pubkey, J, Limit, Out) ->
    B = sign_tx:winner(MyPower, TotalPower, Entropy, Pubkey, J),
    if
        B -> NOut = [J|Out];
        true -> NOut = Out
    end,
    winners(MyPower, TotalPower, Entropy, Pubkey, J+1, Limit, NOut).
             
sign() ->
    Id = keys:id(),
    Acc = account(Id),
    ParentKey = read(top),
    ParentX = read(ParentKey),
    PBlock = ParentX#x.block#signed.data,
    Entropy = PBlock#block.entropy,
    FinalityAcc = accounts:read_account(Id),
    MyPower = min(Acc#acc.delegated, FinalityAcc#acc.delegated),
    TotalPower = PBlock#block.power,
    W = winners(MyPower, TotalPower, Entropy, Acc#acc.pub),
    if 
        length(W) > 0 ->
            Tx = #sign_tx{acc = Id, nonce = Acc#acc.nonce + 1, secret_hash = secrets:new(), winners = W, prev_hash = ParentKey},
            tx_pool:absorb(keys:sign(Tx));
        true ->
            io:fwrite("cannot sign, did not win this round\n")
    end.
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
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = account(Id),
    ChannelPointer = channel(ChannelId),%[-6,"channel",2,0,3]
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, read(top), ChannelId),
    TC = SignedToChannel#signed.data,
    SignedTx = keys:sign(#tc{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, bal1 = TC#tc.bal1 + Inc1, bal2 = TC#tc.bal2 + Inc2, consensus_flag = true, id = ChannelId, fee = Fee, nonce = Acc#acc.nonce + 1, increment = Inc1 + Inc2}),
    #signed{data = SignedTx#signed.data, sig2 = SignedTx#signed.sig2, sig = SignedTx#signed.sig, revealed = ChannelId}.

create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When first creating a new channel, don't add the id. It will be selected for you by next available.    
    Id = keys:id(),
    Acc = account(Id),
    ToAcc = account(To),
    true = Acc#acc.balance > MyBalance,
    true = ToAcc#acc.balance > TheirBalance,
    Tx = #tc{acc1 = Id, acc2 = To, nonce = Acc#acc.nonce + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance},
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
    sign(),
    buy_block(),
    Top = read(read(top)),
    1 = Top#x.block#signed.data#block.power,
    CreateTx1 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx1 = sign:sign_tx(CreateTx1, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx1),
    CreateTx2 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx2 = sign:sign_tx(CreateTx2, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx2),
    CreateTx3 = create_channel(1, 10000, 1000, true, 0),
    SignedCreateTx3 = sign:sign_tx(CreateTx3, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedCreateTx3),
    sign(),
    buy_block(),
    Top2 = read(read(top)),
    1 = Top2#x.block#signed.data#block.power,
    ToChannel = to_channel(0, 0, 10, 0),
    SignedToChannel = sign:sign_tx(ToChannel, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedToChannel),
    sign(),
    buy_block(),
    Top3 = read(read(top)),
    1 = Top3#x.block#signed.data#block.power,
    ChannelTx = close_channel(0, -200, 1),
    TimeoutTx = channel_block(1, -200, 1, 0),
    SlasherTx = channel_block(2, -200, 1, 10),
    SignedChannelTx = sign:sign_tx(ChannelTx, Pub, Priv, dict:new()),
    SignedTimeoutTx = sign:sign_tx(TimeoutTx, Pub, Priv, dict:new()),
    SignedSlasherTx = sign:sign_tx(SlasherTx, Pub, Priv, dict:new()),
    tx_pool:absorb(SignedChannelTx),
    timeout_channel(SignedTimeoutTx),
    timeout_channel(SignedSlasherTx),
    sign(),
    buy_block(),
    Top4 = read(read(top)),
    1 = Top4#x.block#signed.data#block.power,
    SlashBlock = channel_block(2, 0, 2, 5),
    SignedSlashBlock = sign:sign_tx(SlashBlock, Pub, Priv,dict:new()),
    slow_close(1),
    channel_slash(SignedSlashBlock),
    sign(),
    buy_block(),
    Top5 = read(read(top)),
    1 = Top5#x.block#signed.data#block.power,
    success.
