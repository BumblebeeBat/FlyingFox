%When we sign a block, we record the hash of a secret. Later on, we need to reveal this secret.
%This module holds a bunch of secrets, stored in a dict by hash.
-module(secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,delete/1,new/0]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Secret, SH}, X) -> {noreply, dict:store(SH, Secret, X)};
handle_cast({delete, SH}, X) -> {noreply, dict:erase(SH, X)}.
handle_call({read, SH}, _From, X) -> {reply, dict:fetch(SH, X), X}.

new() -> 
    S = crypto:strong_rand_bytes(32),
    SH = hash:doit(S),
    gen_server:cast(?MODULE, {add, S, SH}),
    SH.
read(SH) -> gen_server:call(?MODULE, {read, SH}).
delete(SH) -> gen_server:cast(?MODULE, {del, SH}).
