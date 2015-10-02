-module(channel_block_tx).
-export([doit/4, origin_tx/3, channel/4]).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(bet, {amount = 0, merkle = <<"">>, default = 0}).%signatures
-record(tc, {acc1 = 0, acc2 = 1, nonce1 = 0, nonce2 = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
%`merkle` is the merkle root of a datastructure explaining the bet.
%`default` is the part of money that goes to participant 2 if the bet is still locked when the channel closes. Extra money goes to participant 1.
%There are at least 4 types of bets: hashlock, oracle, burn, and signature. 
creator([], _) -> 1=2;
%creator([Tx|Txs], X) when not is_record(Tx, tc) -> creator(Txs, X);
creator([Tx|_], Id) when Tx#signed.revealed == Id -> Tx;
creator([Tx|_], Id) when Tx#signed.data#timeout.channel_block#signed.data#channel_block.id == Id -> Tx;
creator([_|Txs], Id) -> creator(Txs, Id).
bet_amount(X) -> bet_amount(X, 0).
bet_amount([], X) -> X;
bet_amount([Tx|Txs], X) -> bet_amount(Txs, X+Tx#bet.amount).
origin_tx(BlockNumber, ParentKey, ID) ->
    OriginBlock = block_tree:read_int(BlockNumber, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    %OriginTxs = unwrap_sign(OriginSignedTxs),
    creator(OriginTxs, ID).
doit(Tx, ParentKey, Channels, Accounts) ->
    true = Tx#channel_block.fast,%If fast is false, then you have to use close_channel instead. 
    channel(Tx, ParentKey, Channels, Accounts).
channel(Tx, ParentKey, Channels, Accounts) ->
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
    true = (Tx#channel_block.expiration == 0) or (Tx#channel_block.expiration > CurrentHeight),
    true = (Tx#channel_block.nlock < CurrentHeight),
    N1 = #acc{balance = Acc1#acc.balance + Tx#channel_block.amount,
	      nonce = Acc1#acc.nonce,
	      pub = Acc1#acc.pub},
    N2 = #acc{balance = Acc2#acc.balance + StartAmount - Tx#channel_block.amount,
	      nonce = Acc2#acc.nonce,
	      pub = Acc2#acc.pub},
    MyKey = keys:pubkey(),
    if
	(Acc1#acc.pub == MyKey) or (Acc2#acc.pub == MyKey) -> my_channels:remove(Tx#channel_block.id);
	true -> 1=1
    end,
    NewChannels = dict:store(Tx#channel_block.id, #channel{},Channels),
    NewAccounts1 = dict:store(Tx#channel_block.acc1, N1, Accounts),
    NewAccounts2 = dict:store(Tx#channel_block.acc2, N2, NewAccounts1),
    {NewChannels, NewAccounts2}.

