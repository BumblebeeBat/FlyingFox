-module(port).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, check/0,change/1]).
init(ok) -> {ok, 3010}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({change, X}, _) -> 
    {noreply, X}.
handle_call(check, _From, X) -> {reply, X, X}.

check() -> gen_server:call(?MODULE, check).
change(X) -> 
    true = is_integer(X),
    true = X > 0,
    tcp_server:stop(check()),
    gen_server:cast(?MODULE, {change, X}),
    X = check(),
    listener:die().
    %listener:terminate(0,0).
    
	   
