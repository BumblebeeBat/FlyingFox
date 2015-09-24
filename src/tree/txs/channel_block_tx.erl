-module(channel_block_tx).
-export([doit/4]).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {height = 0, creator = 0, nonce = 0}).
-record(bet, {amount = 0, merkle = <<"">>, default = 0}).%signatures
-record(cc, {acc1 = 0, nonce = 0, acc2 = 1, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
%-record(cc, {acc1 = 0, nonce = 0, acc2 = 1, delay = 10, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
%`merkle` is the merkle root of a datastructure explaining the bet.
%`default` is the part of money that goes to participant 2 if the bet is still locked when the channel closes. Extra money goes to participant 1.
%There are at least 4 types of bets: hashlock, oracle, burn, and signature. 
creator([], _) -> 1=2;
creator([Tx|Txs], X) when not is_record(Tx, cc) -> creator(Txs, X);
creator([Tx|_], Id) when Tx#cc.id == Id -> Tx;
creator([_|Txs], Id) -> creator(Txs, Id).
bet_amount(X) -> bet_amount(X, 0).
bet_amount([], X) -> X;
bet_amount([Tx|Txs], X) -> bet_amount(Txs, X+Tx#bet.amount).
unwrap_sign([]) -> [];
unwrap_sign([Tx|Txs]) -> [Tx#signed.data|unwrap_sign(Txs)].
doit(SignedTx, ParentKey, Channels, Accounts) ->
    Tx = SignedTx#signed.data,
    CurrentHeight = block_tree:height(ParentKey),
    Acc1 = block_tree:account(Tx#channel_block.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#channel_block.acc2, ParentKey, Accounts),
    ChannelPointer = block_tree:channel(Tx#channel_block.id, ParentKey, Channels),
    OriginBlock = block_tree:read_int(ChannelPointer#channel.height, ParentKey),%change read_int to return the block above that height.
    OriginSignedTxs = block_tree:txs(OriginBlock),
    OriginTxs = unwrap_sign(OriginSignedTxs),
    T = hd(tl(tl(OriginTxs))),
    true = T#cc.id == Tx#channel_block.id,
    OriginTx = creator(OriginTxs, Tx#channel_block.id),%err
    AccN1 = Tx#channel_block.acc1,
    AccN1 = OriginTx#cc.acc1,
    AccN2 = Tx#channel_block.acc2,
    AccN2 = OriginTx#cc.acc2,
    true = Tx#channel_block.fast,%If fast is false, then you have to wait 
    StartAmount = OriginTx#cc.bal1 + OriginTx#cc.bal2,
    BetAmount = bet_amount(Tx#channel_block.bets),
    true = Tx#channel_block.amount < (StartAmount - BetAmount + 1),
    true = (Tx#channel_block.expiration == 0) or (Tx#channel_block.expiration > CurrentHeight),
    true = (Tx#channel_block.nlock < CurrentHeight),
    N1 = #acc{balance = Acc1#acc.balance + Tx#channel_block.amount,
	      nonce = Acc1#acc.nonce,
	      pub = Acc1#acc.pub},
    N2 = #acc{balance = Acc2#acc.balance - Tx#channel_block.amount,
	      nonce = Acc2#acc.nonce,
	      pub = Acc2#acc.pub},
    NewChannels = dict:store(Tx#channel_block.id, #channel{},Channels),
    NewAccounts1 = dict:store(Tx#channel_block.acc1, N1, Accounts),
    NewAccounts2 = dict:store(Tx#channel_block.acc2, N2, NewAccounts1),
    {NewChannels, NewAccounts2}.

