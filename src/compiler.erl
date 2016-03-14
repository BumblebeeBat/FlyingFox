-module(compiler).
-export([compile/1, test/0]).

compile(B) ->
    %seperate into list of words.
    %change words to opcode atoms.
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
to_opcodes([<<"{i">>|[I|R]], Functions, Out) ->
    to_opcodes(R, Functions, [{integer, b2i(I)}|Out]);
to_opcodes([<<"{f">>|[T|[B|R]]], Functions, Out) ->
    to_opcodes(R, Functions, [{f, b2i(T), b2i(B)}|Out]);
to_opcodes([<<"false">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [false|Out]);
to_opcodes([<<"true">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [true|Out]);
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
{i 2 square call
">>),
    true = [4] == language:run(A, 1000),
    B = compile(<<"
: square dup * ; 
: quad square call square call ; 
{i 2 quad call
">>),
    true = [16] == language:run(B, 1000),
    C = compile(<<"
: main dup {i 0 > if {i 1 - {i 0 swap recurse call else then ;
{i 5 main call
">>),
    true = [0,0,0,0,0,0] == language:run(C, 1000),
    success.
    
