-module(channel_timeout_tx).
-export([doit/5]).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(channel, {tc = 0, creator = 0, timeout = 0}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.

doit(Tx, ParentKey, Channels, Accounts, BlockGap) ->
    SignedCB = Tx#timeout.channel_block, 
    sign:verify(SignedCB, Accounts),
    CB = SignedCB#signed.data,
    channel_block_tx:channel(CB, ParentKey, Channels, Accounts),
    Acc = block_tree:account(Tx#timeout.acc, ParentKey, Accounts),
    N = #acc{balance = Acc#acc.balance - Tx#timeout.fee,
	     nonce = Acc#acc.nonce + 1,
	     pub = Acc#acc.pub},
    Nonce = N#acc.nonce,
    Nonce = Tx#timeout.nonce,
    true = N#acc.balance > 0,
    Id = CB#channel_block.id,
    N = #acc{balance = Acc#acc.balance - Tx#timeout.fee,
	     nonce = Acc#acc.nonce + 1,
	     pub = Acc#acc.pub},
    NewAccounts = dict:store(Tx#timeout.acc, N, Accounts),
    T = block_tree:read(top),
    Top = block_tree:height(T),
    OldCh = block_tree:channel(Id, ParentKey, Channels),
    Ch = #channel{timeout = Top + BlockGap, creator = Tx#timeout.acc, tc = OldCh#channel.tc},
    NewChannels = dict:store(Id, Ch, Channels),
    {NewChannels, NewAccounts}.
