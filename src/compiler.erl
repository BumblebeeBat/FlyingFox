-module(compiler).
-export([compile/1, test/0]).

compile(B) ->
    C = remove_comments(B),
    Words = to_words(<< <<" ">>/binary, C/binary, <<" ">>/binary>>, <<>>, []),
    Macros = get_macros(Words),
    YWords = remove_macros(Words),
    ZWords = apply_macros(Macros, YWords),
    Functions = get_functions(ZWords),
    AWords = remove_after_define(ZWords),
    BWords = replace_name_function(AWords, Functions),
    to_opcodes(BWords, Functions, []).
remove_comments(B) -> remove_comments(B, <<"">>).
remove_comments(<<"">>, Out) -> Out;
remove_comments(<<37:8, B/binary >>, Out) -> 
    C = remove_till(10, B),
    remove_comments(C, Out);
remove_comments(<<X:8, B/binary>>, Out) -> 
    remove_comments(B, <<Out/binary, X:8>>).
remove_till(N, <<N:8, B/binary>>) -> B;
remove_till(N, <<_:8, B/binary>>) -> 
    remove_till(N, B).
    
remove_macros(Words) -> remove_macros(Words, []).
remove_macros([], Out) -> Out;
remove_macros([<<":m">>|Words], Out) ->
    {_, B} = split(<<";">>, Words),
    remove_macros(B, Out);
remove_macros([W|Words], Out) ->
    remove_macros(Words, Out ++ [W]).
apply_macros(Macros, Words) -> apply_macros(Macros, Words, []).
apply_macros(_, [], Out) -> Out;
apply_macros(Macros, [W|Words], Out) -> 
    NOut = case dict:find(W, Macros) of
	       error -> Out ++ [W];
	       {ok, Val} -> Out ++ Val
	   end,
    apply_macros(Macros, Words, NOut).
get_macros(Words) ->
    get_macros(Words, dict:new()).
get_macros([<<":m">>|[Name|R]], Functions) ->
    %Make sure Name isn't on the restricted list.
    {Code, T} = split(<<";">>, R),
    Code2 = apply_macros(Functions, Code),
    %Opcodes = to_opcodes(Code, Functions, []),
    NewFunctions = dict:store(Name, Code2, Functions),
    get_macros(T, NewFunctions);
get_macros([], Functions) -> Functions;
get_macros([_|T], Functions) -> get_macros(T, Functions).
get_functions(Words) ->
    get_functions(Words, dict:new()).
get_functions([<<":">>|[Name|R]], Functions) ->
    %Make sure Name isn't on the restricted list.
    {Code, T} = split(<<";">>, R),
    Opcodes = to_opcodes(Code, Functions, []),
    NewFunctions = dict:store(Name, hash:doit(Opcodes),Functions),
    get_functions(T, NewFunctions);
get_functions([], Functions) -> Functions;
get_functions([_|T], Functions) -> get_functions(T, Functions).
split(C, B) -> split(C, B, []).
split(C, [C|B], Out) -> {flip(Out), B};
split(C, [D|B], Out) ->
    split(C, B, [D|Out]).
remove_after_define(Words) -> rad(Words, []).
rad([], Out) -> flip(Out);
rad([<<":">>|[_|T]], Out) -> rad(T, [<<":">>|Out]);
rad([X|T], Out) -> rad(T, [X|Out]).
replace_name_function(Words, Functions) ->    
    rnf(Words, Functions, []).
rnf([], _, Out) -> flip(Out);
rnf([H|T], Functions, Out) -> 
    case dict:find(H, Functions) of
	error -> rnf(T, Functions, [H|Out]);
	{ok, Val} -> rnf(T, Functions, [Val|Out])
    end.
b2i(X) -> list_to_integer(binary_to_list(X)).
to_opcodes([<<"binary">>|[B|R]], Functions, Out) ->
    to_opcodes(R, Functions, [base64:decode(B)|Out]);
to_opcodes([<<"integer">>|[I|R]], Functions, Out) ->
    to_opcodes(R, Functions, [{integer, b2i(I)}|Out]);
to_opcodes([<<"fraction">>|[T|[B|R]]], Functions, Out) ->
    to_opcodes(R, Functions, [{f, b2i(T), b2i(B)}|Out]);
to_opcodes([<<"false">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [false|Out]);
to_opcodes([<<"true">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [true|Out]);
to_opcodes([<<"rem">>|R], F, Out) ->
    to_opcodes(R, F, [43|Out]);
to_opcodes([<<"match">>|R], F, Out) ->
    to_opcodes(R, F, [42|Out]);
to_opcodes([<<"recurse">>|R], F, Out) ->
    to_opcodes(R, F, [41|Out]);
to_opcodes([<<"r>">>|R], F, Out) ->
    to_opcodes(R, F, [40|Out]);
to_opcodes([<<">r">>|R], F, Out) ->
    to_opcodes(R, F, [39|Out]);
to_opcodes([<<"r@">>|R], F, Out) ->
    to_opcodes(R, F, [39|[11|[40|Out]]]);%out is all reverse.
to_opcodes([<<"call">>|R], F, Out) ->
    to_opcodes(R, F, [38|Out]);
to_opcodes([<<";">>|R], F, Out) ->
    to_opcodes(R, F, [37|Out]);
to_opcodes([<<":">>|R], F, Out) ->
    to_opcodes(R, F, [36|Out]);
to_opcodes([<<"==">>|R], F, Out) ->
    to_opcodes(R, F, [35|Out]);
to_opcodes([<<"slash">>|R], F, Out) ->
    to_opcodes(R, F, [34|Out]);
to_opcodes([<<"stack_size">>|R], F, Out) ->
    to_opcodes(R, F, [33|Out]);
to_opcodes([<<"height">>|R], F, Out) ->
    to_opcodes(R, F, [32|Out]);
to_opcodes([<<"total_coins">>|R], F, Out) ->
    to_opcodes(R, F, [31|Out]);
to_opcodes([<<"i2f">>|R], F, Out) ->
    to_opcodes(R, F, [30|Out]);
to_opcodes([<<"f2i">>|R], F, Out) ->
    to_opcodes(R, F, [29|Out]);
to_opcodes([<<"crash">>|R], F, Out) ->
    to_opcodes(R, F, [28|Out]);
to_opcodes([<<"flip">>|R], F, Out) ->
    to_opcodes(R, F, [27|Out]);
to_opcodes([<<"stripl">>|R], F, Out) ->
    to_opcodes(R, F, [26|Out]);
to_opcodes([<<"stripr">>|R], F, Out) ->
    to_opcodes(R, F, [25|Out]);
to_opcodes([<<"append">>|R], F, Out) ->
    to_opcodes(R, F, [24|Out]);
to_opcodes([<<"not">>|R], F, Out) ->
    to_opcodes(R, F, [23|Out]);
to_opcodes([<<"xor">>|R], F, Out) ->
    to_opcodes(R, F, [22|Out]);
to_opcodes([<<"or">>|R], F, Out) ->
    to_opcodes(R, F, [21|Out]);
to_opcodes([<<"and">>|R], F, Out) ->
    to_opcodes(R, F, [20|Out]);
to_opcodes([<<"then">>|R], F, Out) ->
    to_opcodes(R, F, [19|Out]);
to_opcodes([<<"else">>|R], F, Out) ->
    to_opcodes(R, F, [18|Out]);
to_opcodes([<<"if">>|R], F, Out) ->
    to_opcodes(R, F, [17|Out]);
to_opcodes([<<"pickn">>|R], F, Out) ->
    to_opcodes(R, F, [16|Out]);
to_opcodes([<<"tuckn">>|R], F, Out) ->
    to_opcodes(R, F, [15|Out]);
to_opcodes([<<"2dup">>|R], F, Out) ->
    to_opcodes(R, F, [14|Out]);
to_opcodes([<<"rot">>|R], F, Out) ->
    to_opcodes(R, F, [13|Out]);
to_opcodes([<<"tuck">>|R], F, Out) ->
    to_opcodes(R, F, [12|Out]);
to_opcodes([<<"dup">>|R], F, Out) ->
    to_opcodes(R, F, [11|Out]);
to_opcodes([<<"drop">>|R], F, Out) ->
    to_opcodes(R, F, [10|Out]);
to_opcodes([<<"swap">>|R], F, Out) ->
    to_opcodes(R, F, [9|Out]);
to_opcodes([<<"=">>|R], F, Out) ->
    to_opcodes(R, F, [8|Out]);
to_opcodes([<<"<">>|R], F, Out) ->
    to_opcodes(R, F, [7|Out]);
to_opcodes([<<">">>|R], F, Out) ->
    to_opcodes(R, F, [6|Out]);
to_opcodes([<<"/">>|R], F, Out) ->
    to_opcodes(R, F, [5|Out]);
to_opcodes([<<"*">>|R], F, Out) ->
    to_opcodes(R, F, [4|Out]);
to_opcodes([<<"-">>|R], F, Out) ->
    to_opcodes(R, F, [3|Out]);
to_opcodes([<<"+">>|R], F, Out) ->
    to_opcodes(R, F, [2|Out]);
to_opcodes([<<"verify_sig">>|R], F, Out) ->
    to_opcodes(R, F, [1|Out]);
to_opcodes([<<"hash">>|R], F, Out) ->
    to_opcodes(R, F, [0|Out]);
to_opcodes([<<"verify_sig_or_die">>|R], F, Out) ->
    X = [19, 18, 28, 17, 23, 1],%in reverse order because Out is all reverse.
    to_opcodes(R, F, X ++ Out);
to_opcodes([<<"commit_reveal">>|[C|[A|R]]], F, Out) ->
    %" integer 3 match D drop integer -1 not if crash else then "
    %This opcode verifies that a hash references a function of the commit-reveal variety. If not, it crashes.
    D  = flip(to_opcodes([C, A], F, [])),%C, A could be a binary, or an integer.
    X = [19, 18, 28, 17, 23, {integer, -1}, 10] ++ D ++ [42, {integer, 3}],
    to_opcodes(R, F, X ++ Out);
to_opcodes([], _, Out) -> flip(Out);
to_opcodes([Name|R], Functions, Out) ->
    case dict:find(Name, Functions) of
	error -> 
	    if
		is_binary(Name) ->
		    to_opcodes(R, Functions, [Name|Out]);
		true ->
		    io:fwrite("undefined word "),
		    io:fwrite(base64:encode(Name)),%looking up a hash.
		    io:fwrite("\n")
	    end;
	{ok, Val} -> 
	    to_opcodes(R, Functions, [Val|Out])
    end.
to_words(<<>>, <<>>, Out) -> flip(Out);
to_words(<<>>, N, Out) -> flip([flip_bin(N)|Out]);
to_words(<<"\t", B/binary>>, X, Out) ->
    to_words(B, X, Out);
to_words(<<" ", B/binary>>, <<"">>, Out) ->
    to_words(B, <<>>, Out);
to_words(<<"\n", B/binary>>, <<"">>, Out) ->
    to_words(B, <<>>, Out);
to_words(<<" ", B/binary>>, N, Out) ->
    to_words(B, <<>>, [flip_bin(N)|Out]);
to_words(<<"\n", B/binary>>, N, Out) ->
    to_words(B, <<>>, [flip_bin(N)|Out]);
to_words(<<C:8, B/binary>>, N, Out) ->
    to_words(B, <<C:8, N/binary>>, Out).
flip_bin(B) -> flip_bin(B, <<>>).
flip_bin(<<>>, Out) -> Out;
flip_bin(<<C:8, B/binary>>, Out) -> 
    flip_bin(B, <<C:8, Out/binary>>).
flip(X) -> flip(X, []).
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
testA0() ->
    %Example of a macro.
    A = compile(<<"
 :m square dup * ; 
 integer 2 square
">>),
    true = [4] == language:run(A, 1000).
testA() ->
    %Example of a function call.
    A = compile(<<"
 : square dup * ; % multiplies a number by itelf
 integer 2 square call
">>),
    true = [4] == language:run(A, 1000).
testB() ->
    %Example of a function using another function, this is how merkle trees work.
    B = compile(<<"
: square dup * ; 
: quad square call square call ; 
integer 2 quad call
">>),
    true = [16] == language:run(B, 1000).
testC() ->
    %Example of a recursive function.
    C = compile(<<"
: main dup integer 0 > if integer 1 - integer 0 swap recurse call else drop then ;
integer 5 main call
">>),
    true = [0,0,0,0,0] == language:run(C, 1000).
testD() ->
    %Example of hashlock code.
    E = compile(<<"
	     binary qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY= hash binary 6DIFJeegWoFCARdzPWKgFaMGniG5vD8Qh+WgPZBb5HQ=  ==
	     ">>),
    [true] = language:run(E, 1000).
testF(EPub, Priv) ->
    %Example of 2 of 3 multisig 
    %The 2 channel owners are the first 2 participants, the pubkey embedded in the script is the third.
    Sig0 = base64:encode(sign:sign(<<"abc">>, Priv)),
    F = compile(<< <<" binary ">>/binary, Sig0/binary,
      <<"    binary YWJj
	     binary ">>/binary,  EPub/binary, <<"
	     verify_sig
	     ">>/binary >>),
    [true] = language:run(F, 1000).
testG(EPub, Priv) ->
% this is a weighted multisig. The first 2 signatures are worth 3, and the last is worth 2. you need 6 total to pass.
    Sig = sign:sign(<<"abc">>, Priv),
    B = compile(<< <<":m b binary YWJj binary ">>/binary, EPub/binary, <<" verify_sig rot ;  b b b
if integer 3 else integer 0 then rot 
if integer 3 else integer 0 then rot 
if integer 2 else integer 0 then 
+ + integer 6 >
       ">>/binary >>),
    [true] = language:run([Sig|[Sig|[Sig|B]]], 1000).
testH(EPub, Priv) -> 
%This is for commit reveal. The nonce for they are required to include, which is custom for this round. it is a very big random number, to avoid collisions, is 1337
%The number they committed to in secret is 55.
    Func = <<" integer 1337 drop integer 55">>,
    C = hash:doit(compile(Func)),
    Sig = base64:encode(sign:sign(C, Priv)),
    D = compile(<< <<" : func " >>/binary, Func/binary, <<" ; 
     binary ">>/binary, Sig/binary,  
 <<" func dup tuck binary ">>/binary, EPub/binary,
      <<" verify_sig_or_die dup commit_reveal integer 1337 
          call ">>/binary >>),
    [55] = language:run(D, 1000).
testI(EPub, Priv) ->
       % here is a contract to punish people for signing contrary results. This contract would be used to stop oracles from outputing 2 contrary results.
   % Sig1, Sig2, func1, func2. <--input top ->
   % func1 != func2
   % Verify that internal pub signed over each func.
   % make sure func1 and func2 are of the correct form.
   % func1(0) != func2(0)
   Func1 = <<" integer 1337 drop integer 55 ">>,
   Func2 = <<" integer 1337 drop integer 54 ">>,
   DFunc1 = << <<" : func1 " >>/binary, Func1/binary, <<" ; ">>/binary >>,
   DFunc2 = << <<" : func2 " >>/binary, Func2/binary, <<" ; ">>/binary >>,
   C1 = hash:doit(compile(Func1)),
   C2 = hash:doit(compile(Func2)),
   Sign1 = base64:encode(sign:sign(C1, Priv)),
   Sign2 = base64:encode(sign:sign(C2, Priv)),
   E = compile(<< DFunc1/binary, DFunc2/binary, <<" binary ">>/binary, Sign1/binary, <<" binary ">>/binary, Sign2/binary, <<" func1 func2 2dup == if crash else then
          2dup commit_reveal integer 1337 
               commit_reveal integer 1337 
	  swap tuck 2dup binary ">>/binary, EPub/binary, 
	  <<" verify_sig_or_die swap drop tuck 2dup binary ">>/binary, EPub/binary, 
          <<" verify_sig_or_die swap drop call
          swap call == if crash else then
	  integer 12345 ">>/binary >>),
    [12345] = language:run(E, 1000).
testJ(EPub, Priv) ->
%This is a weighted multisig with 5 keys.
    Sig = sign:sign(<<"abc">>, Priv),
    B = compile(<< <<":m b binary YWJj binary ">>/binary, EPub/binary, <<" verify_sig >r ;  
:m c r> if else drop integer 0 then ;
b b b b b
integer 3 c
integer 3 c
integer 2 c
integer 2 c
integer 2 c
+ + + + integer 9 >
       ">>/binary >>),
    [true] = language:run([Sig|[Sig|[Sig|[Sig|[Sig|B]]]]], 1000).
testK(EPub, Priv) ->
%This is a weighted multisig that uses the commit-reveal data structure, so that the participants can reveal simultaniously.
%only include signatures from participants who are in the same direction. 
    % input def1 def2 def4 Sig1 Func1 Sig2 Func2 0 Sig4 Func4
    Func = <<" integer 1337 drop integer 1">>,
   DFunc = << <<" : func " >>/binary, Func/binary, <<" ; ">>/binary >>,
    C = hash:doit(compile(Func)),
    Sig = base64:encode(sign:sign(C, Priv)),
    A = compile(<< DFunc/binary, <<" integer 0 ">>/binary, 
      <<" binary ">>/binary, Sig/binary, <<" binary ">>/binary, (base64:encode(C))/binary, 
<<" :m A >r dup integer 0 == if integer 0 >r else dup tuck binary ;
:m B verify_sig_or_die dup commit_reveal integer 1337
  call integer 1 == not if crash else then dup r@ >r then drop ;
integer 3 A ">>/binary, EPub/binary, <<" B 
integer 2 A ">>/binary, EPub/binary, <<" B
integer 0 integer 0
r> + swap r> + swap r> + swap r> + swap 
">>/binary >>),
    [3, 5] = language:run(A, 1000),
    % verify_sig
    % commit_reveal integer 1337
    % function call integer 1 = not if crash else then
    % > 2/3
    ok.

%Concerning the bet nonce. 
%If a lot of validators double-sign, it is important.
%If your partner closes without providing evidence of double-signing, he can make it look like he won when he lost.
%If you provide more information, it should be possible to result in a higher bet-nonce, so you can slash him.

test() ->
    testA(),
    testA0(),
    testB(),
    testC(),
    testD(),
    {Pub, Priv} = sign:new_key(),
    EPub = base64:encode(Pub),
    testF(EPub, Priv),
    testG(EPub, Priv),
    testH(EPub, Priv),
    testI(EPub, Priv),
    testJ(EPub, Priv),
    testK(EPub, Priv),
    success.

% we can use testI contract to remove power in the weighted multisig from any validators who double-sign.

% weighted multisig with merkle identifier. and the ability to ignore anyone who signs on non-identical hashes with identical merkle identifiers.
% the ability to punish people who sign against the majority.
% ability to punish participants who fail to reveal.
% ability to punish participants who reveal early.

%success.

    
%We want a script that uses verify_sig on a hash, and if it verifies, then it calls it. A script like this can be easily modified to do anything else that person wants.
%the function should put a random nonce into the stack and immediately drop it, that way the hash we called wont every be reused, so the signature can't be reused.
