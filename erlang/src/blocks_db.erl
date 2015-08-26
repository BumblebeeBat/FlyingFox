%This only holds blocks that have reached finality. That means that every block in this datastructure is between the current valid block, and the gensis block. Every block in this datastructure is older than the epoch.
%The only way to access the blocks is to apply a function to every one of them in order. Any data we might need from the blocks gets loaded into ram during this pass, so we never have to load an individual block.
-module(blocks_db).
-export([map/2,append/2,test/0]).

append(Filename, Bytes) ->
    case file:open(Filename, [append]) of
        {ok, IoDevice} ->
            file:write(IoDevice, Bytes),
            file:write(IoDevice, "\n"),
            file:close(IoDevice);
        {error, Reason} ->
            io:format("~s open error  reason:~s~n", [Filename, Reason])
    end.

map(Filename, F) -> 
    {ok, Device} = file:open(Filename, [read]),
    for_each_line(Device, F).
for_each_line(Device, F) ->
    case io:get_line(Device, "") of
        eof -> file:close(Device);
        Line -> F(Line),
            for_each_line(Device, F)
    end.

test() ->
    F = "test_file",
    file:write_file(F, ""),
    append(F, "hello"),
    append(F, "world"),
    map(F, fun(X) -> io:fwrite(X) end).

    

