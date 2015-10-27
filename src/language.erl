-module(language).
-export([run/1, test/0, remove_till/2, assemble/1]).

int_arith(2, X, Y) -> X + Y;
int_arith(3, X, Y) -> X - Y;
int_arith(4, X, Y) -> X * Y;
int_arith(5, X, Y) -> X div Y;
int_arith(6, X, Y) -> X > Y;
int_arith(7, X, Y) -> X < Y;
int_arith(8, X, Y) -> X == Y.
frac_arith(2, X, Y) -> fractions:add(X, Y);
frac_arith(3, X, Y) -> fractions:subtract(X, Y);
frac_arith(4, X, Y) -> fractions:multiply(X, Y);
frac_arith(5, X, Y) -> fractions:divide(X, Y);
frac_arith(6, X, Y) -> fractions:less_than(Y, X);
frac_arith(7, X, Y) -> fractions:less_than(X, Y);
frac_arith(8, X, Y) -> fractions:equal(X, Y).
remove_till(X, T) -> remove_till(X, [], T).
remove_till(X, H, [X|T]) -> {flip([X|H]), T};
remove_till(X, H, [A|T]) -> remove_till(X, [A|H], T).
flip(X) -> flip(X, []).
flip([], O) -> O;
flip([H|T], O) -> flip(T, [H|O]).
run(Code) -> run(Code, [], []).
run([], _, Stack) -> Stack;
run([17|Code], UsedCode, [Bool|Stack]) -> %if
    if
	Bool -> run(Code, [17|UsedCode], Stack);
	true -> 
	    {H, T} = remove_till(18, Code),
	    run(T, [17|(H++UsedCode)], Stack)
    end;
run([18|Code], UsedCode, Stack) -> %else
    {H, T} = remove_till(19, Code),
    run(T, [18|(H++UsedCode)], Stack);
run([34|Code], UsedCode, [Start|[Finish|Stack]]) -> %this opcode looks at a section of code that was already processed, and computes the hash of those words. paytoscripthash
    H = list:sublist(UsedCode, Start, Finish),
    O = hash:doit(flip(H)),
    run(Code, [34|UsedCode], [O|Stack]);
run([Word|Code], UsedCode, Stack) ->
     run(Code, [Word|UsedCode], run_helper(Word, Stack)).
run_helper(0, [H|Stack]) -> [hash:doit(H)|Stack];%hash
run_helper(1, [Sig|[Pub|[Data|Stack]]]) ->%verify_sig
    [sign:verify_sig(Data, Sig, Pub)|Stack];
run_helper(Word, [X|[Y|Stack]]) when (is_integer(Word) and ((Word > 1) and (Word < 9))) ->
    Z = if
	(is_integer(X) and is_integer(Y)) ->
	    int_arith(Word, X, Y);
	is_integer(X) ->
	    frac_arith(Word, fractions:new(X, 1), Y);
	is_integer(Y) ->
	    frac_arith(Word, X, fractions:new(Y, 1));
	true ->
	    frac_arith(Word, X, Y)
	end,
    [Z|Stack];
run_helper(9, [X|[Y|Stack]]) -> [Y|[X|Stack]];%swap
run_helper(10, [_|Stack]) -> Stack;%drop
run_helper(11, [X|Stack]) -> [X|[X|Stack]];%dup
run_helper(12, [X|[Y|[Z|Stack]]]) -> [Y|[Z|[X|Stack]]];%rot
run_helper(13, [X|[Y|[Z|Stack]]]) -> [Z|[X|[Y|Stack]]];%-rot (tor)
run_helper(14, [X|[Y|Stack]]) -> [X|[Y|[X|[Y|Stack]]]];%2dup (ddup)
run_helper(15, [N|[X|Stack]]) -> %tuckn 
    H = list:sublist(Stack, 1, N),
    T = list:sublist(Stack, N, 10000000000000000),
    H ++ [X] ++ T;
run_helper(16, [N|Stack]) -> %pickn 
    H = list:sublist(Stack, 1, N),
    T = list:sublist(Stack, N, 10000000000000000),
    [hd(T)] ++ H ++ tl(T);
run_helper(19, Stack) -> Stack;%then 
run_helper(20, [X|[Y|Stack]]) -> [(X and Y)|Stack];%and (both)
run_helper(21, [X|[Y|Stack]]) -> [(X or Y)|Stack];%or (either)
run_helper(22, [X|[Y|Stack]]) -> [(X xor Y)|Stack];%xor (only_one)
run_helper(23, [X|Stack]) -> [(not X)|Stack];%not (invert)
run_helper(24, [X|[Y|Stack]]) -> [<<X/binary, Y/binary>>|Stack];%append binaries
run_helper(25, [X|[Binary|Stack]]) -> %strip right
    T = (size(Binary)*8 - X*8),
    <<A:T, _/binary>> = Binary,
    [<<A:T>>|Stack];
run_helper(26, [X|[Binary|Stack]]) -> %strip left
    Y = X*8,
    <<_:Y, A/binary>> = Binary,
    [A|Stack];
run_helper(27, Stack) -> flip(Stack);
run_helper(28, _) -> 1=2;%crash
run_helper(29, [F|Stack]) -> [fractions:to_int(F)|Stack]; %fraction2int
run_helper(30, [I|Stack]) -> [fractions:new(I, 1)|Stack];%int2fraction
run_helper(31, Stack) -> [block_tree:total_coins()|Stack];%total_caoins
run_helper(32, Stack) -> [block_tree:height()|Stack];%height
run_helper(33, Stack) -> [length(Stack)|Stack];%stack size
run_helper({f, T, B}, Stack) -> [{f, T, B}|Stack];%load fraction into stack.
run_helper(B, Stack) when is_binary(B)-> [B|Stack];%load binary into stack.
run_helper({integer, I}, Stack) -> [I|Stack];%load integer into stack
run_helper(true, Stack) -> [true|Stack];%load binary into stack
run_helper(false, Stack) -> [false|Stack].%load binary into stack

assemble(Code) -> assemble(Code, []).
assemble([], Out) -> flip(Out);
assemble([Word|C], Out) ->
    X = if
	    is_atom(Word) -> atom2op(Word);
	    is_integer(Word) -> {integer, Word};
	    true -> Word
	end,
    assemble(C, [X|Out]).
atom2op(hash) -> 0;
atom2op(verify_sig) -> 1;
atom2op(plus) -> 2;
atom2op(minus) -> 3;
atom2op(multiply) -> 4;
atom2op(divide) -> 5;
atom2op(gt) -> 6;
atom2op(lt) -> 7;
atom2op(eq) -> 8;
atom2op(switch) -> 17;
atom2op(else) -> 18;
atom2op(script_hash) -> 34;
atom2op(swap) -> 9;
atom2op(drop) -> 10;
atom2op(dup) -> 11;
atom2op(rot) -> 12;
atom2op(tor) -> 13;
atom2op(ddup) -> 14;
atom2op(tuckn) -> 15;
atom2op(pickn) -> 16;
atom2op(then) -> 19;
atom2op(both) -> 20;
atom2op(either) -> 21;
atom2op(only_one) -> 22;
atom2op(invert) -> 23;
atom2op(append) -> 24;
atom2op(stripr) -> 25;
atom2op(stripl) -> 26;
atom2op(flip) -> 27;
atom2op(crash) -> 28;
atom2op(f2i) -> 29;
atom2op(i2f) -> 30;
atom2op(total_coins) -> 31;
atom2op(height) -> 32;
atom2op(stack_size) -> 33;
atom2op(true) -> true;
atom2op(false) -> false.

test() ->    
    true = run(assemble([10, 2, plus])) == [12],
    true = run(assemble([{f, 10, 11}, 5, plus])) == [{f, 65, 11}],
    true = run(assemble([false, switch, 100, else, 27, then])) == [27],
    true = run(assemble([true, switch, 100, else, 27, then])) == [100],
    success.

    
