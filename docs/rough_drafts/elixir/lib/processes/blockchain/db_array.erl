-module(db_array).
-export([size/0,get/2,append/1,doit/0]).

size() -> 0.
get(Key, N) -> 0.
append(X) -> 0.

doit() ->
    pwrite("temp", 0, <<"abc">>).
