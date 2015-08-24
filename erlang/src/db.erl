-module(db).
-export([doit/0, save/2, read/1]).

file() -> "database".
save(F, X) -> file:write_file(F, packer:pack(X)).
read(F) ->
    case file:read_file(F) of
        {ok, Out} -> packer:unpack(Out);
        {error, enoent} -> 
            io:fwrite("file does not exist\n"),
            "";
        {error, Reason} -> Reason
    end.
    
doit() ->
    X = <<"{abcd:1}">>,
    save(file(), X),
    X == read(file()).

