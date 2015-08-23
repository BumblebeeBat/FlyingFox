%A limitation of this packer library is that all tuples it encodes must have an atom as the first element. This is done to simplify encoding and decoding records.
%We should add some rules about which atoms can be used with tuples. If peers can trick us into decoding new atoms, they can overflow erlang with too many atoms.
-module(packer).
-export([pack/1,unpack/1,test/0]).
luntup() -> fun(Z) -> untup(Z) end.
untup(X) when is_tuple(X) -> ["tup"|lists:map(luntup(), tuple_to_list(X))];
untup(X) when is_list(X) -> lists:map(luntup(), X);
untup(X) -> X.
lretup() -> fun(Z) -> retup(Z) end.
retup(X) when not is_list(X) -> X;
retup(X) when is_binary(hd(tl(X))) -> list_to_tuple([list_to_atom(binary_to_list(hd(tl(X))))|lists:map(lretup(), tl(tl(X)))]);
retup(X) when hd(X) == "tup" -> list_to_tuple(retup(tl(X)));
retup(X) -> lists:map(lretup(), X).
unpack(JSON) -> unpack_helper(jiffy:decode(JSON)).
unpack_helper(J) when is_list(J) and (hd(J)=="tup") -> 
    retup([hd(J)|unpack_helper(tl(J))]);
unpack_helper(J) when is_list(J) -> lists:map(fun(X) -> unpack_helper(X) end, J);
unpack_helper(J) -> J.
pack(X) when is_tuple(X)-> jiffy:encode(untup(X));
pack(X) when is_list(X)-> jiffy:encode(lists:map(luntup(), X));
pack(X) -> jiffy:encode(X).

-record(p, {p = ""}).
test() -> 
    Tuple = {a, 123, {c}},%is_atom(element(1, Tuple)) = True.
    Record = #p{p=123},
    List = [[],3,[4]],
    Int = 123,
    Bin = <<"abc">>,
    Tuple = retup(untup(Tuple)),
    Complex = [{a, [[{b, 4, []}]]}, Int, Bin],
    true = is_record(retup(untup(Record)), p),
    Int = unpack(pack(Int)),
    Bin = unpack(pack(Bin)),
    List = unpack(pack(List)),
    Tuple = unpack(pack(Tuple)),
    true = is_record(unpack(pack(Record)), p),
    Record = unpack(pack(Record)),
    Complex = unpack(pack(Complex)),
    "success".

    

