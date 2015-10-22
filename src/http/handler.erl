-module(handler).
-export([doit/1]).

doit(X) ->
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
     X.
