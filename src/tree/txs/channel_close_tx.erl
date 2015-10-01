%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_close_tx).
-export([doit/4]).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(channel_close, {acc = 0, nonce = 0, id = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
doit(Tx, ParentKey, Channels, Accounts) ->
    Id = Tx#channel_close.id,
    ChannelPointer = block_tree:channel(Id, ParentKey, Channels),
    SignedOriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    OriginTimeout = SignedOriginTimeout#signed.data,
    OriginTx = OriginTimeout#timeout.channel_block#signed.data,
    T = block_tree:read(top),
    Top = block_tree:height(T),
    true = ChannelPointer#channel.timeout < Top - OriginTx#channel_block.delay + 1,
    channel_block_tx:channel(OriginTx, ParentKey, Channels, Accounts).
