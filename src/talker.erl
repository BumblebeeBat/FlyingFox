-module(talker).
-export([talk/1]).


talk(Msg) ->
    Peer = "http://127.0.0.1:3010/",
    talk(Msg, Peer).
talk(Msg, Peer) ->
    {ok, {_, _, R}} = httpc:request(post, {Peer, [], "application/octet-stream", packer:pack(Msg)}, [], []),
    packer:unpack(R).
    
      
