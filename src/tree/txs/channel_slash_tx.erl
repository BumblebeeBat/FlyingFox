-module(channel_slash_tx).
-export([doit/4]).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
%If you partner tries closing the channel at the wrong point in history, this is how you provide evidence of the true final state
doit(Tx, ParentKey, Channels, Accounts) ->
    SignedCB = Tx#channel_slash.channel_block,
    sign:verify(SignedCB, Accounts),
    CB = SignedCB#signed.data,
    Id = CB#channel_block.id,
    ChannelPointer = block_tree:channel(Id, ParentKey, Channels),
    OriginTimeout = channel_block_tx:origin_tx(ChannelPointer#channel.timeout, ParentKey, Id),
    OriginTx = OriginTimeout#timeout.channel_block#signed.data,
    true = CB#channel_block.nonce > OriginTx#channel_block.nonce,
    channel_block_tx:channel(CB, ParentKey, Channels, Accounts).
