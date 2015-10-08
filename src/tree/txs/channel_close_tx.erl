%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_close_tx).
-export([doit/5, slow_close/1, id/1]).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(channel_close, {acc = 0, nonce = 0, id = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
id(X) -> X#channel_close.id.

doit(Tx, ParentKey, Channels, Accounts, NewHeight) ->
    Id = Tx#channel_close.id,
    ChannelPointer = block_tree:channel(Id, ParentKey, Channels),
    SignedOriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    OriginTimeout = SignedOriginTimeout#signed.data,
    SignedOriginTx = channel_timeout_tx:channel_block(OriginTimeout),
    OriginTx = SignedOriginTx#signed.data,
    T = block_tree:read(top),
    Top = block_tree:height(T),
    true = ChannelPointer#channel.timeout < Top - channel_block_tx:delay(OriginTx) + 1,
    channel_block_tx:channel(OriginTx, ParentKey, Channels, Accounts, NewHeight).
slow_close(Id) ->
    MyId = keys:id(),
    Acc = block_tree:account(MyId),
    tx_pool:absorb(keys:sign(#channel_close{acc = MyId, nonce = accounts:nonce(Acc) + 1, id = Id})).

