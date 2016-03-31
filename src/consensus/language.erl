-module(language).
-export([run/2, test/0, remove_till/2, assemble/1, hashlock/1, extract_sh/1, valid_secret/2, run_script/2, cost/1]).
power(A, B) when B == 1 -> A;
power(A, B) when (B rem 2) == 0 -> power(A*A, B div 2);
power(A, B) -> A*power(A, B-1).
-define(max_integer, power(2, 256)).
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
remove_till(X, H, [X|T], 0) -> {flip(H), T};
remove_till(_, _, _, N) when N < 0 -> N = 0;%N is how many if-statements deep we are.
remove_till(X, H, [17|T], N) -> remove_till(X, [17|H], T, N+1);
remove_till(X, H, [19|T], N) -> remove_till(X, [19|H], T, N-1);
remove_till(X, H, [A|T], N) -> remove_till(X, [A|H], T, N);
remove_till(_, _, X, _) -> 
    io:fwrite("error, you forgot to include 'and' 'else' or 'then' somewhere."),
    X = [0].
match(0, _, Code) -> Code;
match(N, [_|F], [{integer, -1}|Code]) -> 
    match(N - 1, F, Code);
match(N, [C|Function], [C|Code]) -> 
    match(N-1, Function, Code).
flip(X) -> flip(X, []).
flip([], O) -> O;
flip([H|T], O) -> flip(T, [H|O]).
run(Code, Gas) -> run(Code, dict:new(), dict:new(), [], [], Gas).
run(_, _, _, _, _, Gas) when Gas < 0 -> 
    io:fwrite("out of gas"),
    Gas = 0;
run([], _, _, _, Stack, _) -> Stack;
run([17|Code], Functions, Variables, Alt, [Bool|Stack], Gas) -> %if (case)
    X = if
	    Bool -> Code;
	    true -> 
		{_, T} = remove_till(18, Code),
		T
    end,
    run(X, Functions, Variables, Alt, Stack, Gas-cost(17));
run([18|Code], Functions, Variables, Alt, Stack, Gas) -> %else
    {_, T} = remove_till(19, Code),
    run(T, Functions, Variables, Alt, Stack, Gas-cost(18));
run([36|Code], Functions, Variables, Alt, Stack, Gas) ->%define
    {H, T} = remove_till(37, Code),
    B = hash:doit(H),
    run(T, dict:store(B, H, Functions), Variables, Alt, Stack, Gas-cost(36)-length(H));
run([38|Code], Functions, Variables, Alt, [B|Stack], Gas) ->%call
    %Nice for writing scripts.
    %io:fwrite("stack is "),
    %io:fwrite(packer:pack([B|Stack])),
    %io:fwrite("\n"),
    case dict:find(B, Functions) of
	error -> 
	    io:fwrite("is not a function: "),
	    io:fwrite(packer:pack(B)),
	    io:fwrite("\n error in call, known functions: "),
	    io:fwrite(packer:pack(dict:fetch_keys(Functions))),
	    io:fwrite("\n"),
	    io:fwrite("undefined function");
	{ok, F} ->
	    G = replace(41, B, F),%recursion
	    run(G++Code, Functions, Variables, Alt, Stack, Gas-cost(37))
    end;
run([42|Code], Functions, Variables, Alt, [N|[B|Stack]], Gas) ->%match
    case dict:find(B, Functions) of
	error -> 
	    io:fwrite("error in match, known functions: "),
	    io:fwrite(packer:pack(dict:fetch_keys(Functions))),
	    io:fwrite("\n"),
	    io:fwrite("undefined function");
	{ok, F} ->
	    NewCode = match(length(F), F, Code),
	    N = length(Code) - length(NewCode),
	    run(NewCode, Functions, Variables, Alt, [true|Stack], Gas-cost(42))
    end;
run([39|Code], Functions, Variables, Alt, [N|Stack], Gas) ->%moves the top of the stack to the top of the alt stack.
    run(Code, Functions, Variables, [N|Alt], Stack, Gas-cost(39));
run([40|Code], Functions, Vars, [N|Alt], Stack, Gas) ->%moves the top of the alt stack to the top of the stack.
    run(Code, Functions, Vars, Alt, [N|Stack], Gas - cost(40));
run([28|_], _, _, _, _, _) -> %die. Neither person gets money.
    [delete];
run([44|Code], Functions, Vars, Alt, [Name|[Value|Stack]], Gas) -> %store
    NVars = dict:store(Name, Value, Vars),
    run(Code, Functions, NVars, Alt, Stack, Gas);
run([45|Code], Functions, Vars, Alt, [Name|Stack], Gas) -> %fetch
    X = case dict:find(Name, Vars) of
	error -> 0;
	{ok, Val} -> Val
    end,
    run(Code, Functions, Vars, Alt, [X|Stack], Gas);
run([Word|Code], Functions, Vars, Alt, Stack, Gas) ->
    run(Code, Functions, Vars, Alt, run_helper(Word, Stack), Gas-cost(Word)).
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
run_helper(43, [X |[Y |Stack]]) -> [(Y rem X)|Stack];%check remainder after division of 2 values.
%run_helper(36, Stack) -> Stack;
run_helper({f, T, B}, Stack) -> 
    [{f, T, B}|Stack];%load fraction into stack.
run_helper(B, Stack) when is_binary(B)-> 
    true = size(B) < 300,
    [B|Stack];%load binary into stack.
run_helper({integer, I}, Stack) -> 
    %we should probably have a maximum size.
    [(I rem ?max_integer)|Stack];
run_helper(true, Stack) -> [true|Stack];
run_helper(false, Stack) -> [false|Stack].
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
atom2op(swap) -> 9; %( A B -- B A )
atom2op(drop) -> 10;%( X -- )
atom2op(dup) -> 11;%( X -- X X )
atom2op(rot) -> 12;%( a b c -- c a b ) 
atom2op(tor) -> 13;%( a b c -- b c a )
atom2op(ddup) -> 14;%( a b -- a b a b )
atom2op(tuckn) -> 15;%( X N -- ) inserts X N-deeper into stack.
atom2op(pickn) -> 16;%( Stack N -- Stack Nth-item )
% true switch <<"executed">> else <<"ignored">> then 
% false switch <<"ignored">> else <<"executed">> then 
atom2op(switch) -> 17;% conditional statement
atom2op(else) -> 18;% part of an switch conditional statement
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

atom2op(f2i) -> 29; %( F -- I ) fraction to integer
atom2op(i2f) -> 30; %( I -- F ) integer to fraction
atom2op(total_coins) -> 31; %( -- TotalCoins )
atom2op(height) -> 32; %( -- Height )
atom2op(stack_size) -> 33; %( -- Size )
atom2op(slash) -> 34; %( -- true/false)
atom2op(eq) -> 35; %( X Y -- true/false )
atom2op(define) -> 36; % make a new word out of all the opcodes between here and the next 37.
atom2op(stop) -> 37; %crash.
atom2op(call) -> 38; %Use the binary at the top of the stack to look in our hashtable of defined words. If it exists call the code, otherwise crash.
atom2op(to_r) -> 39; %( V -- )
atom2op(from_r) -> 40; %( -- V )
atom2op(recurse) -> 41; %crash. this word should only be used in the definition of a word.
atom2op(match) -> 42; % Use the binary to look up a defined word. Make sure the word matches the code that follows 'match', otherwise crash.
atom2op(remainder) -> 43; % (A B -- C) only works for integers.
atom2op(store) -> 44; % ( X Y -- )
atom2op(fetch) -> 45; % ( Y -- X )
atom2op(true) -> true; %( -- true )
atom2op(false) -> false. %( -- false )
cost(0) -> 20;
cost(1) -> 20;
cost(36) -> 40;
cost(38) -> 20;
cost(44) -> 10;
cost(45) -> 5;
cost(X) when is_integer(X) -> 1;
cost({f, _, _}) -> 3;
cost(B) when is_binary(B) -> size(B);
cost({integer, _}) -> 3;
cost(true) -> 1;
cost(false) -> 1.
replace(A, B, C) -> replace(A, B, C, []).
replace(_, _, [], Out) -> flip(Out);
replace(A, B, [A|T], Out) -> 
    replace(A, B, T, [B|Out]);
replace(A, B, [H|T], Out) -> 
    replace(A, B, T, [H|Out]).
hashlock(SecretHash) ->
    assemble([hash, SecretHash, eq, switch, {f, 0, 1}, {f, 1, 1}, 2, else, {f, 0, 1}, {f, 0, 1}, 1, then]).
valid_secret(Secret, Script) -> 
   hd(tl(run([Secret] ++ Script, 100))).
extract_sh(Code) -> hd(tl(Code)).
run_script(Code, Gas) ->
    case run(Code, Gas) of
	[delete] ->
	    {0,{f,0,1},{f,1,1}};
	Out ->
   %{nonce, Amount to transfer, Amount to delete}
   % the highest nonced scriptsig is the only valid scriptsig.
	    {hd(Out), hd(tl(Out)), hd(tl(tl(Out)))}
    end.
    
    
test() ->    
    true = run(assemble([10, 2, plus]), 100) == [12],
    true = run(assemble([{f, 10, 11}, 5, plus]), 100) == [{f, 65, 11}],
    true = run(assemble([false, switch, 100, else, 27, then, 3]), 100) == [3, 27],%if
    true = run(assemble([true, switch, 100, else, 27, then, 2]), 50) == [2, 100],%if
    true = run(assemble([true, switch, 100, false, switch, else, then, else, 27, then, 2]), 40) == [2, 100],%if %err
    true = run(assemble([true, switch, 100, else, 27, true, switch, else, then, then, 2]), 100) == [2, 100],%if
    {Pub, Priv} = sign:new_key(),
    Data = <<"example">>,
    Sig = sign:sign(Data, Priv),
    true = sign:verify_sig(Data, Sig, Pub),
    true = run(assemble([Sig] ++ [Data, Pub, verify_sig]), 500) == [true],%3rd party signature
    B = hash:doit(1),
    true = run(assemble([1] ++ [hash, B, eq]), 100) == [true],%normal hashlock
    true = run(assemble([{f, 1, 2}, to_r, from_r]), 100) == [{f, 1, 2}],
    H = [{f, 1, 2}, {integer, 500}, plus],
    HASH = hash:doit(assemble(H)),
    Code = [define]++H++[stop, HASH, call],
    true = [{f,1001,2}] == run(assemble(Code), 100),
    H20 = [dup, rot, plus],
    Hash20 = hash:doit(assemble(H20)),
    H2 = [tor, dup, {integer, 0}, gt, switch, {integer, 1}, minus, rot, Hash20, call, recurse, call, else, then],
    Hash2 = hash:doit(assemble(H2)),
    Code1 = [define] ++ H20 ++ [stop, 
	     define] ++ H2 ++ [stop, 
	     {integer, 5}, {integer, 0}, {integer, 1}, Hash2, call],
    true = [0, 8, 5] == run(assemble(Code1), 1000),
    Two = hash:doit(2),
    HTwo = hash:doit(Two),
    io:fwrite(packer:pack([Two] ++ hashlock(HTwo))),
    io:fwrite("\n"),
    [2,{f,1,1},{f,0,1}] = run([Two] ++ hashlock(HTwo), 1000),
    [1,{f,0,1},{f,0,1}] = run([hash:doit(3)] ++ hashlock(HTwo), 1000),
    H30 = [dup, dup, {integer, 5}, plus, plus, plus],
    Hash3 = hash:doit(assemble(H30)),
    Code3 = [define] ++ H30 ++ [stop,
     Hash3, {integer, 6}, match, dup, dup, {integer, 5}, plus, {integer, -1}, plus], 
    [true] = language:run(assemble(Code3), 1000).
%success.
%assemble([H]) ++ hashlock(HASH),
 %   success.
