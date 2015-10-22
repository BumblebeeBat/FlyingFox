-module(talker).
-export([message/0]).
simple_rpc(X) ->
    %case gen_tcp:connect("localhost", 3010, 
    case gen_tcp:connect("localhost", port:check(), 
			 [binary, {packet, 4}]) of
	{ok, Socket} ->
	    %gen_tcp:send(Socket, [term_to_binary(X)]),
	    gen_tcp:send(Socket, [packer:pack(X)]),
	    wait_reply(Socket);
	E -> E
    end.
wait_reply(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    %Term = binary_to_term(Bin),
 	    Term = packer:unpack(Bin),
	    gen_tcp:close(Socket),
	    Term;
 	{tcp_closed, Socket} ->
	    true
      end.

message() -> simple_rpc({a, 1, 2}).
