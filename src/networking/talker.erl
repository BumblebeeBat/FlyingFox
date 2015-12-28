-module(talker).
-export([talk/1, talk/3, local_talk/1]).

peer(IP, Port) ->
    "http://" ++ inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port) ++ "/".

local_talk(Msg) ->
    Peer = "http://127.0.0.1:3011/",
    talk(Msg, Peer).
talk(Msg) ->
    Peer = "http://127.0.0.1:3010/",
    talk(Msg, Peer).
talk(Msg, Peer) ->
    case httpc:request(post, {Peer, [], "application/octet-stream", packer:pack(Msg)}, [{timeout, 1000}], []) of
	{ok, {_, _, R}} -> 
	    packer:unpack(R);
	{error, timeout} ->
	    {error, timeout}
    end.
talk(Msg, IP, Port) -> talk(Msg, peer(IP, Port)).

    
      
