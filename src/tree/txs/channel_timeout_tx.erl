-module(channel_timeout_tx).
-export([doit/5, timeout_channel/1, channel_block/1]).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.
channel_block(X) -> X#timeout.channel_block.
timeout_channel(ChannelTx) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Tx = #timeout{acc = Id, nonce = accounts:nonce(Acc) + 1, channel_block = keys:sign(ChannelTx)},
    tx_pool:absorb(keys:sign(Tx)).

doit(Tx, ParentKey, Channels, Accounts, NewHeight) ->
    SignedCB = Tx#timeout.channel_block, 
    sign:verify(SignedCB, Accounts),
    CB = SignedCB#signed.data,
    channel_block_tx:channel(CB, ParentKey, Channels, Accounts, NewHeight),
    Acc = block_tree:account(Tx#timeout.acc, ParentKey, Accounts),
    N = accounts:update(Acc, NewHeight, (- Tx#timeout.fee), 0, 1),
    Nonce = accounts:nonce(N),
    Nonce = Tx#timeout.nonce,
    Id = channel_block_tx:id(CB),
    NewAccounts = dict:store(Tx#timeout.acc, N, Accounts),
    OldCh = block_tree:channel(Id, ParentKey, Channels),
    Ch = #channel{timeout = NewHeight, creator = Tx#timeout.acc, tc = OldCh#channel.tc},
    NewChannels = dict:store(Id, Ch, Channels),
    {NewChannels, NewAccounts}.
