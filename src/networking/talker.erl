-module(talker).
-export([talk/1, talk/3]).

peer(IP, Port) ->
    "http://" ++ inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port) ++ "/".

talk(Msg) ->
    Peer = "http://127.0.0.1:3010/",
    talk(Msg, Peer).
talk(Msg, Peer) ->
    {ok, {_, _, R}} = httpc:request(post, {Peer, [], "application/octet-stream", packer:pack(Msg)}, [], []),
    packer:unpack(R).
talk(Msg, IP, Port) -> talk(Msg, peer(IP, Port)).

    
      
