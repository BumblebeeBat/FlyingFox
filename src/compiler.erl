-module(compiler).
-export([compile/1, test/0]).

compile(B) ->
    Words = to_words(<<B/binary, " ">>, <<>>, []),
    Functions = get_functions(Words),
    AWords = remove_after_define(Words),
    BWords = replace_name_function(AWords, Functions),
    to_opcodes(BWords, Functions, []).
get_functions(Words) ->
    get_functions(Words, dict:new()).
get_functions([<<":">>|[Name|R]], Functions) ->
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
to_opcodes([<<":b">>|[B|R]], Functions, Out) ->
    to_opcodes(R, Functions, [base64:decode(B)|Out]);
to_opcodes([<<":i">>|[I|R]], Functions, Out) ->
    to_opcodes(R, Functions, [{integer, b2i(I)}|Out]);
to_opcodes([<<":f">>|[T|[B|R]]], Functions, Out) ->
    to_opcodes(R, Functions, [{f, b2i(T), b2i(B)}|Out]);
to_opcodes([<<"false">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [false|Out]);
to_opcodes([<<"true">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [true|Out]);
to_opcodes([<<"match">>|R], F, Out) ->
    to_opcodes(R, F, [42|Out]);
to_opcodes([<<"recurse">>|R], F, Out) ->
    to_opcodes(R, F, [41|Out]);
to_opcodes([<<"from_r">>|R], F, Out) ->
    to_opcodes(R, F, [40|Out]);
to_opcodes([<<"to_r">>|R], F, Out) ->
    to_opcodes(R, F, [39|Out]);
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

test() ->
    A = compile(<<"
: square dup * ; 
:i 2 square call
">>),
    true = [4] == language:run(A, 1000),
    B = compile(<<"
: square dup * ; 
: quad square call square call ; 
:i 2 quad call
">>),
    true = [16] == language:run(B, 1000),
    C = compile(<<"
: main dup :i 0 > if :i 1 - :i 0 swap recurse call else drop then ;
:i 5 main call
">>),
    true = [0,0,0,0,0] == language:run(C, 1000),
    D = compile(<<"
hash 5 == if :f 0 1 :f 1 1 :i 2 else :f 0 1 
	     :f 1 2 1 then
	     ">>),
    E = compile(<<"
	     :b qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY= hash :b 6DIFJeegWoFCARdzPWKgFaMGniG5vD8Qh+WgPZBb5HQ=  ==
	     ">>),
    [true] = language:run(E, 1000),
%top < pub data sig
{Pub, Priv} = {<<"BDDCYHyNP54PuhacHPyuZiMuXNWys0jVgY00zb5i51StymNpPgxIDQ/T13/KZPgZ+YH/gzsIQTqPoMYmwZfOlU0=">>, <<"otDf5SVKc6z1BWFSe9Bmvfs5eGCN059FpSZeHlj4wBw=">>},
    EPub = base64:encode(Pub),
    Sig0 = base64:encode(sign:sign(<<"abc">>, Priv)),
    F = compile(<< <<" :b ">>/binary, Sig0/binary,
      <<"    :b YWJj
	     :b ">>/binary,  EPub/binary, <<"
	     verify_sig
	     ">>/binary >>),
    [true] = language:run(F, 1000),
test2(Priv, EPub).
test2(Priv, EPub) ->
    Acode = << <<"
	 :b YWJj
	 :b ">>/binary, EPub/binary,
		   <<" verify_sig ">>/binary >>,
    Bcode = << Acode/binary, <<" rot ">>/binary >>,
    A = compile(Acode),
    Sig = sign:sign(<<"abc">>, Priv),
    [true] = language:run([Sig|A], 1000),
    B = compile(<< Bcode/binary, Bcode/binary, Bcode/binary, <<"
if :i 3 else :i 0 then rot 
if :i 3 else :i 0 then rot 
if :i 2 else :i 0 then 
+ + :i 6 >
       ">>/binary >>),
    [true] = language:run([Sig|[Sig|[Sig|B]]], 1000),
%2 of 3 multisig over data <<"abc">> using same pubkey 3 times.
% this is a weighted multisig. The first 2 signatures are worth 3, and the last is worth 2. you need 6 total to pass.

%This is for commit reveal. The nonce for they are required to include, which is custom for this round. it is a very big random number, to avoid collisions, is 1337
%The number they committed to in secret is 1.
%Calling the function with an input of 0 must result in the secret. Callint it with a 1 must result in the big random number.
    Func = <<" :i 0 == if :i 55 else :i 1337 then ">>,
    DFunc = << <<" : func " >>/binary, Func/binary, <<" ; 
		 :b ">>/binary >>,
    C = hash:doit(compile(Func)),
    Sig2 = base64:encode(sign:sign(C, Priv)),
    D = compile(<< DFunc/binary,  Sig2/binary,  
 <<" func dup tuck :b ">>/binary, EPub/binary,
      <<" verify_sig 
	  not if crash else
	  :i 1 swap dup tuck call :i 1337 == 
	  not if crash else
	  dup :i 7 match :i 0 == if :i -1 else :i 1337 then 
	  not if crash else
          :i 0 swap call 
	  then then then
	      ">>/binary >>),
   language:run(D, 1000),
   % here is a contract to punish people for signing contrary results. This contract would be used to stop oracles from outputing 2 contrary results.
% we can use this contract to remove power in the weighted multisig from any validators who double-sign.
   % Sig1, Sig2, func1, func2. <--input top ->
   % func1 != func2
   % Verify that internal pub signed over each func.
   % func1(1) == func2(1) == internal constant.
   % func1(0) != func2(0)
   Func1 = <<" :i 0 == if :i 55 else :i 1337 then ">>,
   Func2 = <<" :i 0 == if :i 54 else :i 1337 then ">>,
   DFunc1 = << <<" : func1 " >>/binary, Func1/binary, <<" ; ">>/binary >>,
   DFunc2 = << <<" : func2 " >>/binary, Func2/binary, <<" ; ">>/binary >>,
   C1 = hash:doit(compile(Func1)),
   C2 = hash:doit(compile(Func2)),
   Sign1 = base64:encode(sign:sign(C1, Priv)),
   Sign2 = base64:encode(sign:sign(C2, Priv)),
   E = compile(<< DFunc1/binary, DFunc2/binary, <<" :b ">>/binary, Sign1/binary, <<" :b ">>/binary, Sign2/binary, <<" func1 func2 2dup == if crash else
	  swap tuck 2dup :b ">>/binary, EPub/binary,
	  <<" verify_sig not if crash else
	  swap drop tuck 2dup :b ">>/binary, EPub/binary, 
          <<" verify_sig not if crash else
          swap drop 
     :i 0 swap call
     swap :i 0 swap call == if crash else
	  :i 12345
 then then then then then ">>/binary >>),
     language:run(E, 1000),
% commit reveal, so we reveal ton of bets at the same time, so SVD is possible.
    F = compile(<< <<" :b ">>/binary, (base64:encode(C1))/binary, <<" :b ">>/binary, Sign1/binary, 
<<" swap :b ">>/binary, EPub/binary, <<" verify_sig ">>/binary >>),
    [true] = language:run(F, 1000).

% weighted multisig with merkle identifier. and the ability to ignore anyone who signs on non-identical hashes with identical merkle identifiers.
% the ability to punish people who sign against the majority.
% ability to punish participants who fail to reveal.
% ability to punish participants who reveal early.

%success.

    
%We want a script that uses verify_sig on a hash, and if it verifies, then it calls it. A script like this can be easily modified to do anything else that person wants.
%the function should put a random nonce into the stack and immediately drop it, that way the hash we called wont every be reused, so the signature can't be reused.
