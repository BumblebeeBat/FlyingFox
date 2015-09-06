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
-record(spend, {from=0, to=0, amount=0}).
-record(channel_block, {amount = 0, addressInt1 = 1, addressInt2 = 1}).
-record(sign, {}).
-record(slasher, {}).
-record(reveal, {}).
-record(to_channel, {}).
-record(close_channel, {}).
digest([], _, Accounts, Channels) -> {Accounts, Channels};
digest([Tx|Txs], ParentKey, Accounts, Channels) ->
    true = 
        if
            is_record(Tx, channel_block) ->
                Acc1 = Tx#channel_block.addressInt1,
                Acc2 = Tx#channel_block.addressInt2,
                {Pub1, _, _} = block_tree:account(Acc1, ParentKey),
                {Pub2, _, _} = block_tree:account(Acc2, ParentKey),
                true = sign:verify_both(Tx, Pub1, Pub2),
                true;
            true -> 
                Acc = element(2, Tx), 
                {Pub, _, Nonce} = block_tree:account(Acc, ParentKey),
                true = sign:verify_1(Tx, Pub),
                Nonce = element(3, Tx),
                true
        end,
    
    %check nonce on tx, channel_block is different.
    %when creating this digest, we need to use the block's parent as the root node when looking up accounts and channels from the database.
    {NewAccounts, NewChannels} = 
        if
            is_record(Tx, spend) -> spend_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);
            is_record(Tx, sign) -> sign_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);%use hashmath to make sure validators are valid.
            is_record(Tx, slasher) -> slasher_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);
            is_record(Tx, reveal) -> reveal_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);
            is_record(Tx, to_channel) -> to_channel_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);
            is_record(Tx, channel_block) -> channel_block_tx:doit(Tx, Txs, ParentKey, Accounts, Channels);
            is_record(Tx, close_channel) -> close_channel_tx:doit(Tx, Txs, ParentKey, Accounts, Channels)
        end,
    %make sure there is no negative money in NewAccounts or NewChannel
    digest(Txs, ParentKey, NewAccounts, NewChannels).

test() -> 0.
