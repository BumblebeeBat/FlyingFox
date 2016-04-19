-module(compiler).
-export([compile/1]).

%-define(or_die, [19, 18, 28, 17, 23]).%in reverse order because Out is all reverse. 
-define(or_die, compile(<<" not if crash else then ">>)).
-define(commit_reveal, compile(<<" swap dup integer 2 match integer -10 integer -10 or_die call rot == or_die ">>)).% -10 is the code for any integer.
   % here is code to punish people for signing contrary results. This contract would be used to stop oracles from outputing 2 contrary results.
   % func1 != func2
   % Verify that internal pub signed over each func.
   % make sure func1 and func2 are of the correct form.
   % func1(0) != func2(0)
-define(double_signed_slash, compile(<<"
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die
">>)).
-define(plus_store, compile(<<" dup @ rot + swap ! ">>)).
compile(A) ->
    %Test to make sure : and ; are alternating the whole way, or give an intelligent error.
    B = << <<" ">>/binary, A/binary, <<" \n">>/binary>>,
    C = remove_comments(B),
    D = add_spaces(C),
    Words = to_words(D, <<>>, []),
    Macros = get_macros(Words),
    YWords = remove_macros(Words),
    ZWords = apply_macros(Macros, YWords),
    Functions = get_functions(ZWords),
    AWords = remove_after_define(ZWords),
    BWords = replace_name_function(AWords, Functions),
    to_opcodes(BWords, Functions, []).
add_spaces(B) -> add_spaces(B, <<"">>).
add_spaces(<<"">>, B) -> B;
add_spaces(<<91:8, B/binary >>, Out) ->  % "["
    add_spaces(B, <<Out/binary, 32:8, 91:8, 32:8>>);
add_spaces(<<93:8, B/binary >>, Out) ->  % "]"
    add_spaces(B, <<Out/binary, 32:8, 93:8, 32:8>>);
add_spaces(<<58:8, B/binary >>, Out) ->  % ":"
    add_spaces(B, <<Out/binary, 32:8, 58:8, 32:8>>);
add_spaces(<<59:8, B/binary >>, Out) ->  % ";"
    add_spaces(B, <<Out/binary, 32:8, 59:8, 32:8>>);
add_spaces(<<44:8, B/binary >>, Out) ->  % ","
    add_spaces(B, <<Out/binary, 32:8, 44:8, 32:8>>);
add_spaces(<<X:8, B/binary >>, Out) -> 
    add_spaces(B, <<Out/binary, X:8>>).

remove_comments(B) -> remove_comments(B, <<"">>).
remove_comments(<<"">>, Out) -> Out;
remove_comments(<<40:8, B/binary >>, Out) -> % [40] == ")".
    C = remove_till(41, B), % [41] == ")".
    remove_comments(C, Out);
remove_comments(<<37:8, B/binary >>, Out) -> % [37] == "%".
    C = remove_till(10, B),
    remove_comments(C, Out);
remove_comments(<<X:8, B/binary>>, Out) -> 
    remove_comments(B, <<Out/binary, X:8>>).
remove_till(N, <<N:8, B/binary>>) -> B;
remove_till(N, <<_:8, B/binary>>) -> 
    remove_till(N, B).
    
remove_macros(Words) -> remove_macros(Words, []).
remove_macros([], Out) -> Out;
remove_macros([<<"macro">>|Words], Out) ->
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
get_macros([<<"macro">>|[Name|R]], Functions) ->
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
to_opcodes([<<"++">>|R], F, Out) ->
    to_opcodes(R, F, [54|Out]);
to_opcodes([<<"swap">>|R], F, Out) ->
    to_opcodes(R, F, [53|Out]);
to_opcodes([<<"[">>|R], F, Out) ->
    to_opcodes(R, F, [51|Out]);
to_opcodes([<<"nil">>|R], F, Out) ->
    to_opcodes(R, F, [51|Out]);
to_opcodes([<<"cdr">>|R], F, Out) ->
    to_opcodes(R, F, [50|Out]);
to_opcodes([<<"car">>|R], F, Out) ->
    to_opcodes(R, F, [49|Out]);
to_opcodes([<<"cons">>|R], F, Out) ->
    to_opcodes(R, F, [48|Out]);
to_opcodes([<<",">>|R], F, Out) ->
    to_opcodes(R, F, [48|[53|Out]]);
to_opcodes([<<"]">>|R], F, Out) ->
    to_opcodes(R, F, [52|[48|[53|Out]]]);
to_opcodes([<<"reverse">>|R], F, Out) ->
    to_opcodes(R, F, [52|Out]);
to_opcodes([<<"gas">>|R], F, Out) ->
    to_opcodes(R, F, [47|Out]);
to_opcodes([<<"print">>|R], F, Out) ->
    to_opcodes(R, F, [46|Out]);
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
to_opcodes([<<"@">>|R], F, Out) ->
    to_opcodes(R, F, [45|Out]);
to_opcodes([<<"!">>|R], F, Out) ->
    to_opcodes(R, F, [44|Out]);
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
to_opcodes([<<"^">>|R], F, Out) ->
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
to_opcodes([<<"or_die">>|R], F, Out) ->
    %( bool -- )
    %if bool is true, ignore. if bool is false, then crash.
    to_opcodes(R, F, flip(?or_die) ++ Out);
to_opcodes([<<"commit_reveal">>|R], F, Out) ->
    %( Function -- )
    %This opcode verifies that a hash references a function of the commit-reveal variety. If not, it crashes.
    to_opcodes(R, F, flip(?commit_reveal) ++ Out);
to_opcodes([<<"+!">>|R], F, Out) ->
    %( 5 N -- ) This increments N by 5.
    to_opcodes(R, F, flip(?plus_store) ++ Out);
to_opcodes([<<"double_signed_slash">>|R], F, Out) ->
    %( Sig1 Sig1 Func1 Func2 Pub identifier_constant -- )
    %This macro can tell if someone double-signed
    to_opcodes(R, F, flip(?double_signed_slash) ++ Out);
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
