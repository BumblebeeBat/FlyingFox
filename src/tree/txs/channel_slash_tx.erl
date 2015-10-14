-module(channel_slash_tx).
-export([doit/6, channel_slash/1, channel_block/1]).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
%-record(channel, {tc = 0, creator = 0, timeout = 0}).
%-record(channel, {acc1 = 0, acc2 = 0, bal1 = 0, bal2 = 0, called_timeout = 0, called_timeout_nonce = 0, timeout_height = 0, step = empty}).%step is either: delegated_1, delegated_2, non_delegated, or timeout
%If you partner tries closing the channel at the wrong point in history, this is how you provide evidence of the true final state
channel_block(Tx) ->
    Tx#channel_slash.channel_block.
channel_slash(ChannelTx) ->
    MyId = keys:id(),
    channel_slash(ChannelTx, MyId).
channel_slash(ChannelTx, MyId) ->
    Acc = block_tree:account(MyId),
    tx_pool:absorb(keys:sign(#channel_slash{acc = MyId, nonce = accounts:nonce(Acc), channel_block = ChannelTx})).
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    SignedCB = Tx#channel_slash.channel_block,
    sign:verify(SignedCB, Accounts),
    CB = sign:data(SignedCB),
    Id = channel_block_tx:id(CB),
    Channel = block_tree:channel(Id, ParentKey, Channels),
    case channels:called_timeout(Channel) of
	0 -> A = Tx#channel_slash.acc, A = channels:acc2(Channel);
	1 -> A = Tx#channel_slash.acc, A = channels:acc1(Channel)
    end,
    OriginTimeout = channel_block_tx:origin_tx(channels:timeout_height(Channel), ParentKey, Id),
    SignedOriginTx = channel_timeout_tx:channel_block(sign:data(OriginTimeout)),
    OriginTx = sign:data(SignedOriginTx),
    true = channel_block_tx:nonce(CB) > channel_block_tx:nonce(OriginTx),
    channel_block_tx:channel(CB, ParentKey, Channels, Accounts, TotalCoins, NewHeight).
