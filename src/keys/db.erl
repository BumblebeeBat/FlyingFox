-module(db).
-export([test/0, save/2, read/1]).
-define(file, "database.db").
save(F, X) -> file:write_file(F, packer:pack(X)).
read(F) ->
    case file:read_file(F) of
        {ok, Out} -> packer:unpack(Out);
        {error, enoent} -> 
            io:fwrite("file does not exist\n"),
            "";
        {error, Reason} -> Reason
    end.
-record(d, {a = "", b = "" }).
test() ->
    X = #d{a=[1, 2, <<"abc">>, []], b = <<1,2,3,200>> },
    save(?file, X),
    X = read(?file),
    success.

