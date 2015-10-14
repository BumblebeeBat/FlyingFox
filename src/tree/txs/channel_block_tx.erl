-module(channel_block_tx).
-export([doit/6, origin_tx/3, channel/6, channel_block/4, cc_losses/1, close_channel/3, id/1, delay/1, nonce/1]).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
%-record(channel, {acc1 = 0, acc2 = 0, bal1 = 0, bal2 = 0, called_timeout = 0, called_timeout_nonce = 0, timeout_height = 0, step = empty}).%step is either: delegated_1, delegated_2, non_delegated, or timeout
%-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(bet, {amount = 0, merkle = <<"">>, default = 0}).%signatures
-record(tc, {acc1 = 0, acc2 = 0, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
%`merkle` is the merkle root of a datastructure explaining the bet.
%`default` is the part of money that goes to participant 2 if the bet is still locked when the channel closes. Extra money goes to participant 1.
%There are at least 4 types of bets: hashlock, oracle, burn, and signature. 
nonce(X) -> X#channel_block.nonce.
id(X) -> X#channel_block.id.
delay(X) -> X#channel_block.delay.
close_channel(Id, Amount, Nonce) ->
    Channel = block_tree:channel(Id),
    %ChannelPointer = block_tree:channel(Id),
    %SignedToChannel = origin_tx(ChannelPointer#channel.tc, block_tree:read(top), Id),
    %TC = sign:data(SignedToChannel),
    %keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = true}).
    keys:sign(#channel_block{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), amount = Amount, nonce = Nonce, id = Id, fast = true}).
cc_losses(Txs) -> cc_losses(Txs, 0).%filter out channel_block, channel_slash, and channel_close type txs. add up the amount of money in each such channel. Exclude channels that Haven't been open since finality.
cc_losses([], X) -> X;
cc_losses([SignedTx|T], X) -> 
    Tx = sign:data(SignedTx),
    case element(1, Tx) of
	channel_block ->
	    %ParentKey = block_tree:read(top),
	    Channel = block_tree:channel(Tx#channel_block.id, dict:new()),
	    StartAmount = channels:bal1(Channel) + channels:bal2(Channel),
	    FChannel = channels:read_channel(Tx#channel_block.id),
	    S = channels:type(Channel),
	    %false = S == timeout,%I guess channel needs a 3-way switch, and a 2-way switch. It needs to remember which type of delegated it is until the end.
	    A1 = channels:acc1(Channel),
	    A2 = channels:acc2(Channel),
	    B1 = channels:acc1(FChannel),
	    B2 = channels:acc2(FChannel),
	    if
		non_delegated == S -> SA = 0;
		(A1 == B1) and (A2 == B2) ->
		    SA = StartAmount;
		true -> SA = 0
	    end,
	    cc_losses(T, X+SA);
	    %ChannelPointer = block_tree:channel(Tx#channel_block.id, dict:new()),
	%SignedOriginTx = origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
	%OriginTx = sign:data(SignedOriginTx),
	%StartAmount = OriginTx#tc.bal1 + OriginTx#tc.bal2,
	    
	%FChannelPointer = channels:read_channel(Tx#channel_block.id),
	%FSignedOriginTx = channel_block_tx:origin_tx(FChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
	%FOriginTx = sign:data(FSignedOriginTx),
	%if
						%if channel consensus flag is off, then SA = 0
	%(FOriginTx#tc.acc1 == OriginTx#tc.acc1) and
	%(FOriginTx#tc.acc2 == OriginTx#tc.acc2) ->
	%SA = StartAmount;
	%true -> SA = 0
	%end,
	%cc_losses(T, X+SA);
	channel_slash ->
	    cc_losses([channel_slash_tx:channel_block(Tx)|T], X);
	channel_close ->
	    ParentKey = block_tree:read(top),
	    Id = channel_close_tx:id(Tx),
	    ChannelPointer = block_tree:channel(Id, dict:new()),
	    SignedOriginTimeout = channel_block_tx:origin_tx(channels:timeout_height(ChannelPointer), ParentKey, Id),
	    OriginTimeout = sign:data(SignedOriginTimeout),
	    CB = channel_timeout_tx:channel_block(OriginTimeout),
	    cc_losses([CB|T], X);
	_ -> cc_losses(T, X)
    end.
    
creator([], _) -> sign:empty(#tc{});
creator([SignedTx|T], Id) ->
    Tx = sign:data(SignedTx),
    R = sign:revealed(SignedTx),
    Type = element(1, Tx),
    if
	R == Id -> SignedTx;
	Type == timeout ->
	    SignedCB = channel_timeout_tx:channel_block(Tx),
	    CB = sign:data(SignedCB),
	    I = CB#channel_block.id,
	    if 
		I == Id -> SignedTx;
		true -> creator(T, Id)
	    end;
	true ->
	    creator(T, Id)
    end.
bet_amount(X) -> bet_amount(X, 0).
bet_amount([], X) -> X;
bet_amount([Tx|Txs], X) -> bet_amount(Txs, X+Tx#bet.amount).
channel_block(Id, Amount, Nonce, Delay) ->
    Channel = block_tree:channel(Id),
    keys:sign(#channel_block{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), amount = Amount, nonce = Nonce, id = Id, fast = false, delay = Delay}).
%ChannelPointer = block_tree:channel(Id),
%SignedToChannel = channel_block_tx:origin_tx(ChannelPointer#channel.tc, block_tree:read(top), Id),
%TC = sign:data(SignedToChannel),
%keys:sign(#channel_block{acc1 = TC#tc.acc1, acc2 = TC#tc.acc2, amount = Amount, nonce = Nonce, id = Id, fast = false, delay = Delay}).
origin_tx(BlockNumber, ParentKey, ID) ->%this should also include a type tag, right???
    %it should be 2 functions. one for timeout, and one for signed?
    OriginBlock = block_tree:read_int(BlockNumber, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    %OriginTxs = unwrap_sign(OriginSignedTxs),
    creator(OriginTxs, ID).
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    true = Tx#channel_block.fast,%If fast is false, then you have to use close_channel instead. 
    channel(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight).

channel(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    Acc1 = block_tree:account(Tx#channel_block.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#channel_block.acc2, ParentKey, Accounts),
    Channel = block_tree:channel(Tx#channel_block.id, ParentKey, Channels),
    FChannel = channels:read_channel(Tx#channel_block.id),
    AccN1 = Tx#channel_block.acc1,
    AccN1 = channels:acc1(Channel),
    AccN2 = Tx#channel_block.acc2,
    AccN2 = channels:acc2(Channel),
    StartAmount = channels:bal1(Channel) + channels:bal2(Channel),
    BetAmount = bet_amount(Tx#channel_block.bets),
    true = Tx#channel_block.amount + BetAmount < StartAmount + 1,
    true = BetAmount - Tx#channel_block.amount < StartAmount + 1,
    true = (Tx#channel_block.expiration == 0) or (Tx#channel_block.expiration > NewHeight),    
    true = (Tx#channel_block.nlock < NewHeight),
    A1 = Tx#channel_block.acc1,
    A2 = Tx#channel_block.acc2,
    B1 = channels:acc1(FChannel),
    B2 = channels:acc2(FChannel),
    Type = channels:type(Channel),
    if
	%It hasn't been open since finality -> D1 = 0, D1 = 0;
	not ((B1 == A1) and
	(B2 == A2)) ->
	    D2 = 0,
	    D1 = 0;
	delegated_1 == Type ->
	    io:fwrite("bad \n"),
	    D1 = StartAmount,
	    D2 = 0;
	Type == delegated_2 ->
	    D2 = StartAmount,
	    D1 = 0;
	Type == non_delegated ->
	    D2 = 0,
	    D1 = 0
    end,
    N1 = accounts:update(Acc1, NewHeight, channels:bal1(Channel) + Tx#channel_block.amount, -D1, 0, TotalCoins),
    N2 = accounts:update(Acc2, NewHeight, channels:bal2(Channel) - Tx#channel_block.amount, -D2, 0, TotalCoins),
    MyKey = keys:pubkey(),
    APub1 = accounts:pub(Acc1),
    APub2 = accounts:pub(Acc2),
    if
	(APub1 == MyKey) or (APub2 == MyKey) -> my_channels:remove(Tx#channel_block.id);
	true -> 1=1
    end,
    NewChannels = dict:store(Tx#channel_block.id, channels:empty(),Channels),
    NewAccounts1 = dict:store(Tx#channel_block.acc1, N1, Accounts),
    NewAccounts2 = dict:store(Tx#channel_block.acc2, N2, NewAccounts1),
    {NewChannels, NewAccounts2, TotalCoins}.%remove money from totalcoins that was deleted in bets.

%ChannelPointer = block_tree:channel(Tx#channel_block.id, ParentKey, Channels),
%SignedOriginTx = origin_tx(ChannelPointer#channel.tc, ParentKey, Tx#channel_block.id),
%OriginTx = sign:data(SignedOriginTx),
%AccN1 = Tx#channel_block.acc1,
%AccN1 = OriginTx#tc.acc1,
%AccN2 = Tx#channel_block.acc2,
%AccN2 = OriginTx#tc.acc2,
%StartAmount = OriginTx#tc.bal1 + OriginTx#tc.bal2,
%BetAmount = bet_amount(Tx#channel_block.bets),
%true = Tx#channel_block.amount + BetAmount < StartAmount + 1,
%true = BetAmount - Tx#channel_block.amount < StartAmount + 1,
%true = (Tx#channel_block.expiration == 0) or (Tx#channel_block.expiration > CurrentHeight),
%true = (Tx#channel_block.nlock < CurrentHeight),
    %one of their delegations should decrease, since we are closing the channel. Depends on OriginTx#tc.consensus_flag...
%if
%OriginTx#tc.consensus_flag ->
%D1 = StartAmount,
%D2 = 0;
%true ->
%D1 = 0,
%D2 = StartAmount
%end,
    %update height in each account, and have them pay fees for delegation.
%N1 = accounts:update(Acc1, NewHeight, OriginTx#tc.bal1 + Tx#channel_block.amount, -D1, 0, TotalCoins),
%N2 = accounts:update(Acc2, NewHeight, OriginTx#tc.bal2 - Tx#channel_block.amount, -D2, 0, TotalCoins),
%$MyKey = keys:pubkey(),
%APub1 = accounts:pub(Acc1),
%APub2 = accounts:pub(Acc2),
%if
%(APub1 == MyKey) or (APub2 == MyKey) -> my_channels:remove(Tx#channel_block.id);
%true -> 1=1
%end,
%NewChannels = dict:store(Tx#channel_block.id, #channel{},Channels),
%NewAccounts1 = dict:store(Tx#channel_block.acc1, N1, Accounts),
%NewAccounts2 = dict:store(Tx#channel_block.acc2, N2, NewAccounts1),
%{NewChannels, NewAccounts2, TotalCoins}.%remove money from totalcoins that was deleted in bets.

