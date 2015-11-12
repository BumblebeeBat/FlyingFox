-module(language).
-export([run/1, test/0, remove_till/2, assemble/1, hashlock/2, extract_sh/1, valid_secret/2]).
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
remove_till(X, T) -> remove_till(X, [], T, 0).
remove_till(X, H, [X|T], 0) -> {flip([X|H]), T};
remove_till(_, _, _, N) when N < 0 -> 1 = 0;%N is how many if-statements deep we are.
remove_till(X, H, [17|T], N) -> remove_till(X, [17|H], T, N+1);
remove_till(X, H, [19|T], N) -> remove_till(X, [19|H], T, N-1);
remove_till(X, H, [A|T], N) -> remove_till(X, [A|H], T, N);
remove_till(_, _, [], _) -> 
    io:fwrite("error, you forgot to include and else or then somewhere."),
    1=0.
flip(X) -> flip(X, []).
flip([], O) -> O;
flip([H|T], O) -> flip(T, [H|O]).
run(Code) -> run(Code, [], []).
run([], _, Stack) -> Stack;
run([17|Code], UsedCode, [Bool|Stack]) -> %if (case)
    if
	Bool -> run(Code, [17|UsedCode], Stack);
	true -> 
	    {H, T} = remove_till(18, Code),
	    run(T, [17|(H++UsedCode)], Stack)
    end;
run([18|Code], UsedCode, Stack) -> %else
    {H, T} = remove_till(19, Code),
    run(T, [18|(H++UsedCode)], Stack);
run([36|Code], UsedCode, [Start|[Size|Stack]]) -> %this opcode looks at a section of code that was already processed, and computes the hash of those words. paytoscripthash
    %This measures backwards.
    %example A, B, C, 3, 0, script_hash takes the hash of [A, B, C]
    H = lists:sublist(UsedCode, Start, Size),
    %io:fwrite(flip(H)),%[2,{integer,3},{integer,1}],[])
    O = hash:doit(flip(H)),
    run(Code, [36|UsedCode], [O|Stack]);
run([28|_], _, _) -> %die. Neither person gets money.
    [delete];
run([Word|Code], UsedCode, Stack) ->
    run(Code, [Word|UsedCode], run_helper(Word, Stack)).
run_helper(0, [H|Stack]) -> 
    [hash:doit(H)|Stack];%hash
run_helper(1, [Pub|[Data|[Sig|Stack]]]) ->%verify_sig
    [sign:verify_sig(Data, Sig, Pub)|Stack];
run_helper(Word, [Y|[X|Stack]]) when (is_integer(Word) and ((Word > 1) and (Word < 9))) ->
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
run_helper(29, [F|Stack]) -> [fractions:to_int(F)|Stack]; %fraction2int
run_helper(30, [A|[B|Stack]]) -> [fractions:new(B, A)|Stack];%int2fraction
run_helper(31, Stack) -> [block_tree:total_coins()|Stack];%total_caoins
run_helper(32, Stack) -> [block_tree:height()|Stack];%height
run_helper(33, Stack) -> [length(Stack)|Stack];%stack size
run_helper(34, Stack) -> [false|Stack];%this returns true if called from a channel_slash tx.
run_helper(35, [X |[Y |Stack]]) -> [(X == Y)|Stack];%check if 2 non-numerical values are equal. like binary.
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
atom2op(hash) -> 0;%( X -- <<Bytes:256>> )
atom2op(verify_sig) -> 1;%( Sig Data Pub -- true/false )
atom2op(plus) -> 2;%( X Y -- Z )
atom2op(minus) -> 3;%( X Y -- Z )
atom2op(multiply) -> 4;%( X Y -- Z )
atom2op(divide) -> 5;%( X Y -- Z )
atom2op(gt) -> 6;%( X Y -- true/false )
atom2op(lt) -> 7;%( X Y -- true/false )
atom2op(eq_num) -> 8;%( X Y -- true/false )
atom2op(switch) -> 17;% conditional statement
% true switch <<"executed">> else <<"ignored">> then 
% false switch <<"ignored">> else <<"executed">> then 
atom2op(else) -> 18;% part of an switch conditional statement
atom2op(swap) -> 9; %( A B -- B A )
atom2op(drop) -> 10;%( X -- )
atom2op(dup) -> 11;%( X -- X X )
atom2op(rot) -> 12;%( a b c -- c a b ) 
atom2op(tor) -> 13;%( a b c -- b c a )
atom2op(ddup) -> 14;%( a b -- a b a b )
atom2op(tuckn) -> 15;%( X N -- ) inserts X N-deeper into stack.
atom2op(pickn) -> 16;%( Stack N -- Stack Nth-item )
atom2op(then) -> 19;%part of switch conditional statement.
atom2op(both) -> 20;%( true/false true/false -- true/false )
atom2op(either) -> 21;%( true/false true/false -- true/false )
atom2op(only_one) -> 22;%( true/false true/false -- true/false )
atom2op(invert) -> 23;%( true/false -- false/true )
atom2op(append) -> 24;%( <<Binary1/binary>> <<Binary2/binary>> -- <<Binary1/binary, Binary2/binary>> )
atom2op(stripr) -> 25;%( <<Binary/binary>> -- <<ShorterBinary/binary>> )
atom2op(stripl) -> 26;%( <<Binary/binary>> -- <<ShorterBinary/binary>> )
atom2op(flip) -> 27;%entire stack is flipped.
atom2op(crash) -> 28;%code stops execution here. Neither person gets the money.

atom2op(f2i) -> 29; %( F -- I )
atom2op(i2f) -> 30; %( I -- F )
atom2op(total_coins) -> 31; %( -- TotalCoins )
atom2op(height) -> 32; %( -- Height )
atom2op(stack_size) -> 33; %( -- Size )
atom2op(slash) -> 34; %( -- true/false)
atom2op(eq) -> 35; %( X Y -- true/false )
atom2op(scripthash) -> 36; %( size start -- <<Bytes:256>> )
%this opcode looks at a section of code that was already processed, and computes the hash of those words. paytoscripthash

atom2op(true) -> true;
atom2op(false) -> false.

hashlock(ToAmount, SecretHash) ->
    true = ((ToAmount == 0) or (ToAmount == 1)),
    assemble([hash, SecretHash, eq, switch, {f, ToAmount, 1}, 2, else, {f, 1, 2}, 1, then]).
valid_secret(Secret, Script) -> 
    %Bool = (hash:doit(Secret) == hd(tl(Script))),
    io:fwrite("script "),
    io:fwrite(packer:pack(Script)),
    io:fwrite("\n"),
    Amount = hd(tl(run([Secret] ++ Script))),
    fractions:to_int(Amount).

extract_sh(Code) -> hd(tl(Code)).
    
test() ->    
    true = run(assemble([10, 2, plus])) == [12],
    true = run(assemble([{f, 10, 11}, 5, plus])) == [{f, 65, 11}],
    true = run(assemble([false, switch, 100, else, 27, then, 3])) == [3, 27],%if
    true = run(assemble([true, switch, 100, else, 27, then, 2])) == [2, 100],%if
    true = run(assemble([true, switch, 100, false, switch, else, then, else, 27, then, 2])) == [2, 100],%if %err
    true = run(assemble([true, switch, 100, else, 27, true, switch, else, then, then, 2])) == [2, 100],%if
    {Pub, Priv} = sign:new_key(),
    Data = <<"example">>,
    Sig = sign:sign(Data, Priv),
    true = sign:verify_sig(Data, Sig, Pub),
    true = run(assemble([Sig] ++ [Data, Pub, verify_sig])) == [true],%3rd party signature
    B = <<169,243,219,139,234,91,46,239,146,55,229,72,9,221,164,63,12,33,143,128,208,211,40,163,63,91,76,255,255,51,72,230>>,%hash of 1.
    true = run(assemble([1] ++ [hash, B, eq])) == [true],%normal hashlock
    Code = [2, 2, plus],
    ScriptHash = hash:doit(assemble(Code)),
    true = (run(assemble([27] ++ Code ++ [length(Code),3,scripthash])) == [ScriptHash, 4, 27]),%pay2scripthash
    %(i) merkle tree transform to cut the size of the state, and (ii) lightning networks at the same time
    C = [true, 2, drop],
    CodeHash = hash:doit(assemble(C)),
    ScriptPubkey = [switch, length(C), 4, scripthash, CodeHash, eq, swap, hash, B, eq, both, else, then],
    ScriptKey1 = [false],
    ScriptKey2 = [1, true, 2, drop],
    true = run(assemble(ScriptKey1 ++ ScriptPubkey)) == [],
    true = run(assemble(ScriptKey2 ++ ScriptPubkey)) == [true],
    success.
