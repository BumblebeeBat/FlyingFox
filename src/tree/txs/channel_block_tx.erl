-module(channel_block_tx).
-export([doit/5, origin_tx/3, channel/5, channel_block/4, cc_losses/1, close_channel/3, id/1, delay/1, nonce/1]).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(bet, {amount = 0, merkle = <<"">>, default = 0}).%signatures
-record(tc, {acc1 = 0, acc2 = 0, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
%`merkle` is the merkle root of a datastructure explaining the bet.
%`default` is the part of money that goes to participant 2 if the bet is still locked when the channel closes. Extra money goes to participant 1.
%There are at least 4 types of bets: hashlock, oracle, burn, and signature. 
nonce(X) -> X#channel_block.nonce.
id(X) -> X#channel_block.id.
delay(X) -> X#channel_block.delay.
close_channel(Id, Amount, Nonce) ->
    ChannelPointer = block_tree:channel(Id),
    SignedToChannel = origin_tx(ChannelPointer#channel.tc, block_tree:read(top), Id),
    TC = SignedToChannel#signed.data,
    keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = true}).
cc_losses(Txs) -> cc_losses(Txs, 0).%filter out channel_block, channel_slash, and channel_close type txs. add up the amount of money in each such channel. Exclude channels that Haven't been open since finality.
cc_losses([], X) -> X;
cc_losses([#signed{data = Tx}|T], X) when is_record(Tx, channel_block) -> 
    ParentKey = block_tree:read(top),
    ChannelPointer = block_tree:channel(Tx#channel_block.id, dict:new()),
    SignedOriginTx = origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    OriginTx = SignedOriginTx#signed.data,
    StartAmount = OriginTx#tc.bal1 + OriginTx#tc.bal2,

    FChannelPointer = channels:read_channel(Tx#channel_block.id),
    FSignedOriginTx = channel_block_tx:origin_tx(FChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    FOriginTx = FSignedOriginTx#signed.data,
    if
	%if channel consensus flag is off, then SA = 0
        (FOriginTx#tc.acc1 == OriginTx#tc.acc1) and
        (FOriginTx#tc.acc2 == OriginTx#tc.acc2) ->
            SA = StartAmount;
        true -> SA = 0
    end,
    cc_losses(T, X+SA);
cc_losses([#signed{data = Tx}|T], X) when element(1, Tx) == channel_slash -> cc_losses([channel_slash_tx:channel_block(Tx)|T], X);
cc_losses([#signed{data = Tx}|T], X) when element(1,Tx) == channel_close -> 
    ParentKey = block_tree:read(top),
    Id = channel_close_tx:id(Tx),
    %Id = Tx#channel_close.id,
    ChannelPointer = block_tree:channel(Id, dict:new()),
    SignedOriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    OriginTimeout = SignedOriginTimeout#signed.data,
    CB = channel_timeout_tx:channel_block(OriginTimeout),
    cc_losses([CB|T], X);
cc_losses([_|T], X) -> cc_losses(T, X).

creator([], _) -> #signed{data = #tc{}};
creator([Tx = #signed{revealed = Id}|_], Id) -> Tx;
creator([Tx = #signed{data = X}|T], Id) when element(1, X) == timeout -> 
    CB = channel_timeout_tx:channel_block(X),
    I = CB#signed.data#channel_block.id,
    if 
	I == Id -> Tx;
	true -> creator(T, Id)
    end;
creator([_|Txs], Id) -> creator(Txs, Id).
bet_amount(X) -> bet_amount(X, 0).
bet_amount([], X) -> X;
bet_amount([Tx|Txs], X) -> bet_amount(Txs, X+Tx#bet.amount).
channel_block(Id, Amount, Nonce, Delay) ->
    ChannelPointer = block_tree:channel(Id),
    SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, block_tree:read(top), Id),
    TC = SignedToChannel#signed.data,
    keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = false, delay = Delay}).
origin_tx(BlockNumber, ParentKey, ID) ->
    OriginBlock = block_tree:read_int(BlockNumber, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    %OriginTxs = unwrap_sign(OriginSignedTxs),
    creator(OriginTxs, ID).
doit(Tx, ParentKey, Channels, Accounts, NewHeight) ->
    true = Tx#channel_block.fast,%If fast is false, then you have to use close_channel instead. 
    channel(Tx, ParentKey, Channels, Accounts, NewHeight).

channel(Tx, ParentKey, Channels, Accounts, NewHeight) ->
    CurrentHeight = block_tree:height(ParentKey),
    Acc1 = block_tree:account(Tx#channel_block.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#channel_block.acc2, ParentKey, Accounts),
    ChannelPointer = block_tree:channel(Tx#channel_block.id, ParentKey, Channels),
    SignedOriginTx = origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
    OriginTx = SignedOriginTx#signed.data,
    AccN1 = Tx#channel_block.acc1,
    AccN1 = OriginTx#tc.acc1,
    AccN2 = Tx#channel_block.acc2,
    AccN2 = OriginTx#tc.acc2,
    StartAmount = OriginTx#tc.bal1 + OriginTx#tc.bal2,
    BetAmount = bet_amount(Tx#channel_block.bets),
    true = Tx#channel_block.amount + BetAmount < StartAmount + 1,
    true = BetAmount - Tx#channel_block.amount < StartAmount + 1,
    true = (Tx#channel_block.expiration == 0) or (Tx#channel_block.expiration > CurrentHeight),
    true = (Tx#channel_block.nlock < CurrentHeight),
    %one of their delegations should decrease, since we are closing the channel. Depends on OriginTx#tc.consensus_flag...
    if
        OriginTx#tc.consensus_flag ->
            D1 = StartAmount,
            D2 = 0;
        true ->
            D1 = 0,
            D2 = StartAmount
    end,
    %update height in each account, and have them pay fees for delegation.
    N1 = accounts:update(Acc1, NewHeight, OriginTx#tc.bal1 + Tx#channel_block.amount, -D1, 0),
    N2 = accounts:update(Acc2, NewHeight, OriginTx#tc.bal2 - Tx#channel_block.amount, -D2, 0),
    MyKey = keys:pubkey(),
    APub1 = accounts:pub(Acc1),
    APub2 = accounts:pub(Acc2),
    if
	(APub1 == MyKey) or (APub2 == MyKey) -> my_channels:remove(Tx#channel_block.id);
	true -> 1=1
    end,
    NewChannels = dict:store(Tx#channel_block.id, #channel{},Channels),
    NewAccounts1 = dict:store(Tx#channel_block.acc1, N1, Accounts),
    NewAccounts2 = dict:store(Tx#channel_block.acc2, N2, NewAccounts1),
    {NewChannels, NewAccounts2}.

