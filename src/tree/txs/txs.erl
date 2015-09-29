%each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.

-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,txs/0,digest/5,test/0]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("txs died!"), ok.
handle_info(_, X) -> {noreply, X}.

handle_call(txs, _From, X) -> {reply, X, X}.
handle_cast(dump, _) -> {noreply, []};
handle_cast({add_tx, Tx}, X) -> {noreply, [Tx|X]}.
dump() -> gen_server:cast(?MODULE, dump).
txs() -> gen_server:call(?MODULE, txs).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0}).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(sign, {}).
-record(slasher, {}).
-record(reveal, {}).
%-record(close_channel, {}).
-record(ca, {from = 0, nonce = 0, to = 0, pub = <<"">>, amount = 0}).
-record(da, {from = 0, nonce = 0, to = <<"0">>}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(tc, {acc1 = 0, acc2 = 1, nonce1 = 0, nonce2 = 0, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
%-record(channel_slash, {acc = 0, nonce = 0, id = 0, channel_block = 0}).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0}).
-record(channel_close, {acc = 0, nonce = 0, id = 0}).


%-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
%-record(tc, {acc1 = 0, nonce = 0, acc2 = 1, bal1 = 0, bal2 = 0, consensus_flag = false, id = 0, fee = 0}).
digest([], _, Channels, Accounts, _) -> {Channels, Accounts};
digest([SignedTx|Txs], ParentKey, Channels, Accounts, BlockGap) ->
    true = sign:verify(SignedTx, Accounts),
    Tx = SignedTx#signed.data,
    {NewChannels, NewAccounts} = 
        if
            is_record(Tx, ca) -> create_account_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, spend) -> spend_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, da) -> delete_account_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, sign) -> sign_tx:doit(Tx, ParentKey, Channels, Accounts);%use hashmath to make sure validators are valid.
            is_record(Tx, slasher) -> slasher_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, reveal) -> reveal_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, tc) -> to_channel_tx:doit(Tx, ParentKey, Channels, Accounts, BlockGap);
            is_record(Tx, channel_block) -> channel_block_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, timeout) -> channel_timeout_tx:doit(Tx, ParentKey, Channels, Accounts, BlockGap);
            is_record(Tx, channel_slash) -> channel_slash_tx:doit(Tx, ParentKey, Channels, Accounts);
            is_record(Tx, channel_close) -> channel_close_tx:doit(Tx, ParentKey, Channels, Accounts);
            true -> 
		io:fwrite(packer:pack(Tx)),
		1=2
        end,
    digest(Txs, ParentKey, NewChannels, NewAccounts, BlockGap).

test() -> 0.
