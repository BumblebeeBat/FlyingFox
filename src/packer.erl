%We should add some rules about which atoms can be used with tuples. If peers can trick us into decoding new atoms, they can overflow erlang with too many atoms.
-module(packer).
-export([pack/1,unpack/1,test/0, untup/1, unpack_helper/1]).
-define(KEY, -6).
untup(X) when is_tuple(X) -> [?KEY|untup(tuple_to_list(X))];
untup([]) -> [];
untup([H|T]) -> [untup(H)|untup(T)];
untup(X) when is_list(X) -> lists:map(fun(Z)->untup(Z) end, X);
untup(X) when is_binary(X) -> base64:encode(X);
untup(X) -> X.
unpack(JSON) -> unpack_helper(jiffy:decode(JSON)).
unpack_helper(J) when is_binary(J) -> base64:decode(J);
unpack_helper(J) when not is_list(J) -> J;
unpack_helper(J) when hd(J) == ?KEY -> list_to_tuple([binary_to_atom(hd(tl(J)), latin1)|unpack_helper(tl(tl(J)))]);
unpack_helper(J) -> lists:map(fun(X) -> unpack_helper(X) end, J).
pack(X) when is_binary(X) -> base64:encode(X);
pack(X) when is_tuple(X) or is_list(X) -> jiffy:encode(untup(X));
pack(X) -> jiffy:encode(X).

-record(d, {a = "", b = "" }).
test() -> 
    Record = #d{a=[1, 2, <<"abc">>, []], b = <<1,2,3,200>> },
    List = [[],3,[4]],
    Int = 123,
    Bin = <<"abc">>,
    Int = unpack(pack(Int)),
    Bin = unpack_helper(pack(Bin)),
    List = unpack(pack(List)),
    true = is_record(unpack(pack(Record)), d),
    Record = unpack(pack(Record)),
    true = is_binary(pack(Record)),
    "success".
