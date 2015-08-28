-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,add_tx/1,txs/0,add_tx_helper/1]).
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
    Prev_hash = "",
    Tx = {},
    true = valid_tx(Tx, txs(), Prev_hash),
    gen_server:cast(?MODULE, {add_tx, Tx}).
add_tx(Tx) -> spawn(txs, add_tx_helper, Tx).
-record(tx, {t="", d=""}).
valid_tx(Tx, Txs, Prev_hash) ->
    case Tx#tx.t of
        <<"spend">> -> spend_tx:check(Tx#tx.d, Txs);
        <<"sign">> -> sign_tx:check(Tx#tx.d, Txs);
        <<"slasher">> -> sign_tx:check(Tx#tx.d, Txs);
        <<"reveal">> -> reveal_tx:check(Tx#tx.d, Txs);
        <<"to_channel">> -> to_channel_tx:check(Tx#tx.d, Txs);
        <<"channel_block">> -> channel_block_tx:check(Tx#tx.d, Txs);
        <<"close_channel">> -> close_channel_tx:check(Tx#tx.d, Txs)
    end.
        
