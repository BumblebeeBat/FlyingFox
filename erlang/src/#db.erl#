-module(db).
-export([doit/0]).

file() -> "database".
save(F, X) -> file:write_file(F, X).
read(F) ->
    case file:read_file(F) of
        {ok, Out} -> Out;
        {error, enoent} -> io:fwrite("file does not exist\n");
        {error, Reason} -> Reason
    end.
    
doit() ->
    X = <<"{abcd:1}">>,
    save(file(), X),
    X == read(file()).
%binary_to_list(Out).
