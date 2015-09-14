-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,add_tx/1,txs/0,add_tx_helper/1,digest/4,test/0]).
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
add_tx_helper(Tx) ->
    _Prev_hash = "",
    Tx = {},
    %true = valid_tx(Tx, txs(), Prev_hash),
    %get digest as it currently is, and try growing it with the new tx.
    %make sure new digest pieces don't have negative money.
    %run checks from block_tester.
    gen_server:cast(?MODULE, {add_tx, Tx}).
add_tx(Tx) -> spawn(txs, add_tx_helper, Tx).
%-record(tx, {pub = "", t="", d=""}).
-record(channel_block, {amount = 0, addressInt1 = 1, addressInt2 = 1}).
-record(spend, {from=0, to=0, nonce = 0, amount=0}).
-record(sign, {}).
-record(slasher, {}).
-record(reveal, {}).
-record(to_channel, {}).
-record(close_channel, {}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
digest([], _, Accounts, Channels) -> {Accounts, Channels};
digest([SignedTx|Txs], ParentKey, Accounts, Channels) ->
    Tx = SignedTx#signed.data,
    if
        is_record(Tx, channel_block) ->
	    N1 = element(2, Tx), 
            N2 = element(3, Tx), 
            Acc1 = block_tree:account(N1, ParentKey, Accounts),
            Acc2 = block_tree:account(N2, ParentKey, Accounts),
            true = sign:verify_both(SignedTx, Acc1#acc.pub, Acc2#acc.pub);
	true -> 
	    N = element(2, Tx),
            Acc = block_tree:account(N, ParentKey, Accounts),
	    io:fwrite(packer:pack(Acc)),
	    true = sign:verify_1(SignedTx, Acc#acc.pub) %err here!!
    end,
    {NewAccounts, NewChannels} = 
        if
            is_record(Tx, spend) -> spend_tx:doit(Tx, ParentKey, Accounts, Channels);
            is_record(Tx, sign) -> sign_tx:doit(Tx, ParentKey, Accounts, Channels);%use hashmath to make sure validators are valid.
            is_record(Tx, slasher) -> slasher_tx:doit(Tx, ParentKey, Accounts, Channels);
            is_record(Tx, reveal) -> reveal_tx:doit(Tx, ParentKey, Accounts, Channels);
            is_record(Tx, to_channel) -> to_channel_tx:doit(Tx, ParentKey, Accounts, Channels);
            is_record(Tx, channel_block) -> channel_block_tx:doit(Tx, ParentKey, Accounts, Channels);
            is_record(Tx, close_channel) -> close_channel_tx:doit(Tx, ParentKey, Accounts, Channels);
            true -> 1=2
        end,
    digest(Txs, ParentKey, NewAccounts, NewChannels).

test() -> 0.
