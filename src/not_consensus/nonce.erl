%The purpose of this is to keep track of nonce between us and each server, and if we are a server, it keeps track of our customer's nonces.
%customer_get(Id), server_get(Id)
-module(nonce).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, customer_get/1,server_get/1,customer_next/1,server_next/1,test/0]).
init(ok) -> {ok, {dict:new(), dict:new()}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({server_next, Id}, {C, S}) -> 
    N = case dict:find(Id, S) of
	    error -> 1;
	    {ok, Val} -> Val+1
	end,
    {noreply, {C, dict:store(Id, N, S)}};
handle_cast({customer_next, Id}, {C, S}) -> 
    N = case dict:find(Id, C) of
	    error -> 1;
	    {ok, Val} -> Val+1
	end,
    {noreply, {dict:store(Id, N, C), S}}.
handle_call({customer_get, Id}, _From, {C, S}) -> 
    N = case dict:find(Id, C) of
	    error -> 0;
	    {ok, Val} -> Val
	end,
    {reply, N, {C, S}};
handle_call({server_get, Id}, _From, {C, S}) -> 
    N = case dict:find(Id, S) of
	    error -> 0;
	    {ok, Val} -> Val
	end,
    {reply, N, {C, S}}.

customer_get(ID) -> gen_server:call(?MODULE, {customer_get, ID}).
server_get(ID) -> gen_server:call(?MODULE, {server_get, ID}).
customer_next(ID) -> gen_server:cast(?MODULE, {customer_next, ID}).
server_next(ID) -> gen_server:cast(?MODULE, {server_next, ID}).
    
test() ->
    Z = 0,
    Z = customer_get(1),
    Z = customer_get(0),
    Z = server_get(1),
    Z = server_get(0),
    customer_next(1),
    customer_next(0),
    server_next(1),
    server_next(0),
    One = 1,
    One = customer_get(1),
    One = customer_get(0),
    One = server_get(1),
    One = server_get(0),
    success.
    
