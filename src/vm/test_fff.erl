-module(test_fff).
-export([doit/2, test/0]).
-define(loc, "src/vm/fff/").
doit(X, Gas) ->
    {ok, A} = file:read_file(?loc ++ X ++ ".fs"),
    B = compiler:compile(<<A/binary, <<" test ">>/binary >>),
    language:run(B, Gas).

test() ->
    [[13,10,5,4,2]] = doit("sort", 5000),
    X = doit("oracle", 50000),
    X = oracle:test(),
    [10] = doit("reduce", 1000),
    [[25,36,49]] = doit("map", 1000),
    doit("growing_database", 1000),
    [true] = doit("hashlock", 1000),
    [0,0,0,0,0] = doit("recursion", 1000),
    [16] = doit("function", 1000),
    [4] = doit("macro", 1000),
    [11, 10, 12, 12] = doit("variable", 1000),
    success.
    
