-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1,dump/0,accounts/0,channels/0,test/0]).
-record(f, {txs = [], accounts = dict:new(), channels = dict:new()}).
init(ok) -> {ok, #f{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(accounts, _From, F) -> {reply, F#f.accounts, F};
handle_call(channels, _From, F) -> {reply, F#f.channels, F}.
handle_cast(dump, _) -> {noreply, #f{}};
handle_cast({absorb, Tx, Channels, Accounts}, F) -> 
    {noreply, #f{txs = [Tx|F#f.txs], accounts = Accounts, channels = Channels}}.
dump() -> gen_server:cast(?MODULE, dump).
accounts() -> gen_server:call(?MODULE, accounts).
channels() -> gen_server:call(?MODULE, channels).
absorb(Tx) -> 
    Accounts = accounts(),
    Channels = channels(),
    {NewChannels, NewAccounts} = txs:digest([Tx], block_tree:read(top), Channels, Accounts, 1),%Usually blocks are one after the other. Some txs may have to get removed if we change this number to a 2 before creating the block.
    gen_server:cast(?MODULE, {absorb, Tx, NewChannels, NewAccounts}).

-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
%-record(ca, {from = 0, nonce = 0, to = 0, pub = <<"">>, amount = 0}).
test() ->
    {Pub, _Priv} = sign:new_key(),
    CreateAccount = keys:sign(#ca{from = 0, nonce = 1, pub=Pub, amount=10000}),
    Spend = keys:sign(#spend{from = 0, nonce = 2, to = 1, amount=10}),
    absorb(CreateAccount),
    absorb(Spend),
    accounts().
