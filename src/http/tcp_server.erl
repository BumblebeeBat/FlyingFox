%% Copyright (C) 2002, Joe Armstrong
%% File    : tcp_server.erl
%% Author  : Joe Armstrong (joe@sics.se)
%% Purpose : Keeps track of a number of TCP sessions
%% Last modified: 2002-11-17

-module(tcp_server).

-export([start_raw_server/4, start_client/3]).
-export([stop/1]).
-export([children/1]).
%% -export([start_child/3]).

%% start_raw_server(Port, Fun, Max)
%%   This server accepts up to Max connections on Port
%%   The *first* time a connection is made to Port
%%   Then Fun(Socket) is called. 
%%   Thereafter messages to the socket result in messsages to the handler.

%% a typical server is usually written like this:

%% To setup a lister

%% start_server(Port) ->    
%%     S = self(),
%%     process_flag(trap_exit, true),
%%     tcp_server:start_raw_server(Port, 
%% 				fun(Socket) -> input_handler(Socket, S) end, 
%% 				15,
%%                              0)
%%     loop().

%% The loop() process is a central controller that all
%% processes can use to synchronize amongst themselfves if necessary
%% It ends up as the variable "Controller" in the input_handler


%% input_handler(Socket, Controller) ->
%%     receive
%% 	{tcp, Socket, Bin} ->
%% 	    ...
%% 	    gen_tcp:send(Socket, ...)
%% 
%% 	{tcp_closed, Socket} ->
%%
%% 
%% 	Any ->
%% 	    ...
%% 
%%     end.

start_client(Host, Port, Length) ->
     gen_tcp:connect(Host, Port,
		     [binary, {active, true}, {length, Length}]).
					 
%% Note when start_raw_server returns it should be ready to
%% Immediately accept connections

start_raw_server(Port, Fun, Max, Length) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Self = self(),
	    Pid = spawn_link(fun() ->
				     cold_start(Self, Port, Fun, Max, Length)
			     end),
	    receive
		{Pid, ok} ->
		    register(Name, Pid),
		    {ok, self()};
		{Pid, Error} ->
		    Error
	    end;
	Pid ->
	    {error, already_started}
    end.

stop(Port) when is_integer(Port) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    not_started;
	Pid ->
	    exit(Pid, kill),
	    (catch unregister(Name)),
	    stopped
    end.

children(Port) when is_integer(Port) ->
    port_name(Port) ! {children, self()},
    receive
	{session_server, Reply} -> Reply
    end.

port_name(Port) when is_integer(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).

cold_start(Master, Port, Fun, Max, Length) ->
    process_flag(trap_exit, true),
    io:format("Starting a port server on ~p...~n",[Port]),
    case gen_tcp:listen(Port, [binary,
			       %% {dontroute, true},
			       {nodelay,true},
			       {packet, Length},
			       {reuseaddr, true}, 
			       {active, false}]) of
	{ok, Listen} ->
	    %% io:format("Listening on:~p~n",[Listen]),
	    Master ! {self(), ok},
	    New = start_accept(Listen, Fun),
	    %% Now we're ready to run
	    socket_loop(Listen, New, [], Fun, Max);
	Error ->
	    Master ! {self(), Error}
    end.

%% Don't mess with the following code uless you really know what you're 
%% doing (and Thanks to Magnus for heping me get it right)

socket_loop(Listen, New, Active, Fun, Max) ->
    receive
	{istarted, New} ->
	    Active1 = [New|Active],
	    possibly_start_another(false, Listen, Active1, Fun, Max);
	{'EXIT', New, Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    possibly_start_another(false, Listen, Active, Fun, Max);
	{'EXIT', Pid, Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    Active1 = lists:delete(Pid, Active),
	    possibly_start_another(New, Listen, Active1, Fun, Max);
	{children, From} ->
	    From ! {session_server, Active},
	    socket_loop(Listen, New, Active, Fun, Max);
	Other ->
	    io:format("Here in loop:~p~n",[Other])
    end.

possibly_start_another(New, Listen, Active, Fun, Max) when pid(New) ->
    socket_loop(Listen, New, Active, Fun, Max);
possibly_start_another(false, Listen, Active, Fun, Max) ->
    case length(Active) of
	N when N < Max ->
	    New = start_accept(Listen, Fun),
	    socket_loop(Listen, New, Active, Fun, Max);
	_ ->
	    socket_loop(Listen, false, Active, Fun, Max)
    end.

start_accept(Listen, Fun) ->
    S = self(),
    spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Parent ! {istarted,self()},		    % tell the controller
	    inet:setopts(Socket, [{nodelay,true},
				  {active, true}]), % before we activate socket
	    %% io:format("running the child:~p~n",[Socket]),
	    Fun(Socket);
	Other ->
	    exit(oops)
    end.









