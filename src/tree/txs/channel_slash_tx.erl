-module(channel_slash_tx).
-export([doit/4, channel_slash/1, channel_block/1]).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
%If you partner tries closing the channel at the wrong point in history, this is how you provide evidence of the true final state
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).
channel_block(Tx) ->
    Tx#channel_slash.channel_block.
channel_slash(ChannelTx) ->
    MyId = keys:id(),
    Acc = block_tree:account(MyId),
    tx_pool:absorb(keys:sign(#channel_slash{acc = MyId, nonce = Acc#acc.nonce, channel_block = ChannelTx})).
doit(Tx, ParentKey, Channels, Accounts) ->
    SignedCB = Tx#channel_slash.channel_block,
    sign:verify(SignedCB, Accounts),
    CB = SignedCB#signed.data,
    Id = channel_block_tx:id(CB),
    ChannelPointer = block_tree:channel(Id, ParentKey, Channels),
    OriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    SignedOriginTx = channel_timeout_tx:channel_block(OriginTimeout#signed.data),
    OriginTx = SignedOriginTx#signed.data,
    true = channel_block_tx:nonce(CB) > channel_block_tx:nonce(OriginTx),
    channel_block_tx:channel(CB, ParentKey, Channels, Accounts).
