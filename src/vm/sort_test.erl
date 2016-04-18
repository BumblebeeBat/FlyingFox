-module(sort_test).
-export([test/0]).

test() ->
    Gas = 5000,
    {ok, A} = file:read_file("src/vm/sort.fs"),
    B = compiler:compile(A),
    language:run(B, Gas).
