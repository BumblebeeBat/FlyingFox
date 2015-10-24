%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_close_tx).
-export([doit/7, slow_close/1, id/1]).
%-record(channel, {acc1 = 0, acc2 = 0, bal1 = 0, bal2 = 0, called_timeout = 0, called_timeout_nonce = 0, timeout_height = 0, step = empty}).%step is either: delegated_1, delegated_2, non_delegated, or timeout
-record(channel_close, {acc = 0, nonce = 0, id = 0}).
id(X) -> X#channel_close.id.

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Id = Tx#channel_close.id,
    Channel = block_tree:channel(Id, ParentKey, Channels),
    SignedOriginTimeout = channel_block_tx:origin_tx(channels:timeout_height(Channel), ParentKey, Id),
    OriginTimeout = sign:data(SignedOriginTimeout),
    SignedOriginTx = channel_timeout_tx:channel_block(OriginTimeout),
    OriginTx = sign:data(SignedOriginTx),
    T = block_tree:read(top),
    Top = block_tree:height(T),
    true = channels:timeout_height(Channel) < Top - channel_block_tx:delay(OriginTx) + 1,
    channel_block_tx:channel(SignedOriginTx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight).
slow_close(Id) ->
    MyId = keys:id(),
    Acc = block_tree:account(MyId),
    tx_pool:absorb(keys:sign(#channel_close{acc = MyId, nonce = accounts:nonce(Acc) + 1, id = Id})).

