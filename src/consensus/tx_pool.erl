-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1,dump/1,secrets/0,accounts/0,channels/0,txs/0,total_coins/0,test/0]).
-record(f, {txs = [], accounts = dict:new(), channels = dict:new(), total_coins = block_tree:total_coins(), secrets = dict:new()}).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
init(ok) -> 
    process_flag(trap_exit, true),
    {ok, #f{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(total_coins, _From, F) -> {reply, F#f.total_coins, F};
handle_call(accounts, _From, F) -> {reply, F#f.accounts, F};
handle_call(channels, _From, F) -> {reply, F#f.channels, F};
handle_call(secrets, _From, F) -> {reply, F#f.secrets, F};
handle_call(txs, _From, F) -> {reply, F#f.txs, F}.
handle_cast({dump, TotalCoins}, _) -> {noreply, #f{total_coins = TotalCoins}};
handle_cast({absorb, SignedTx}, F) ->
%Tx, Channels, Accounts, TotalCoins, Secrets}, F) -> 
    Tx = sign:data(SignedTx),
    Channels = F#f.channels,
    %R = sign:revealed(SignedTx),
    NewTx = if
		(is_record(Tx, tc)) and (Tx#tc.id == -1) ->
	    %select the location for the new channel in the database at the very last possible moment. 
		    Revealed = to_channel_tx:next_top(block_tree:read(top), Channels),
		    sign:set_revealed(SignedTx, Revealed);
		true -> SignedTx
    end,
    H = block_tree:height(),
    if
	element(1, Tx) == sign_tx ->
	    false = sign_tx:repeat(element(2, Tx), F#f.txs);
	true -> 0
    end,
    {NewChannels, NewAccounts, NewTotalCoins, NewSecrets} = txs:digest([NewTx], block_tree:read(top), Channels, F#f.accounts, F#f.total_coins, F#f.secrets, H+1),%Usually blocks are one after the other. Some txs may have to get removed if height increases by more than 1 between adjacent blocks.
    {noreply, #f{txs = [NewTx|F#f.txs], accounts = NewAccounts, channels = NewChannels, total_coins = NewTotalCoins, secrets = NewSecrets}}.
dump(TotalCoins) -> gen_server:cast(?MODULE, {dump, TotalCoins}).
total_coins() -> gen_server:call(?MODULE, total_coins).
accounts() -> gen_server:call(?MODULE, accounts).
channels() -> gen_server:call(?MODULE, channels).
secrets() -> gen_server:call(?MODULE, secrets).
flip(In) -> flip(In, []).
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
txs() -> flip(gen_server:call(?MODULE, txs)).
absorb(SignedTx) -> 
    gen_server:cast(?MODULE, {absorb, SignedTx}).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
test() ->
    {Pub, _Priv} = sign:new_key(),
    CreateAccount = keys:sign(#ca{from = 0, nonce = 1, pub=Pub, amount=12020}),
    Spend = keys:sign(#spend{from = 0, nonce = 2, to = 1, amount=122}),
    absorb(CreateAccount),
    absorb(Spend),
    accounts().
