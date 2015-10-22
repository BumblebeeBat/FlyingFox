-module(listener).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, die/0]).
init(ok) -> 
    %spawn_link(fun() -> server(3010) end),
    spawn_link(fun() -> server(port:check()) end),
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    %tcp_server:stop(3010),
    tcp_server:stop(port:check()),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(die, X) -> 1=2, {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
die() -> gen_server:cast(?MODULE, die).

server(Port) -> tcp_server:start_raw_server(Port, fun(Socket) -> input_handler(Socket) end, 15, 4).
input_handler(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    %Term = binary_to_term(Bin),
 	    Term = packer:unpack(Bin),
	    Reply = the_func(Term),
 	    send_term(Socket, Reply),
	    input_handler(Socket);
 	{tcp_closed, Socket} ->
	    true
     end.
send_term(Socket, Term) ->
    %gen_tcp:send(Socket, [term_to_binary(Term)]).
    gen_tcp:send(Socket, [packer:pack(Term)]).
the_func(X)  ->  X.
