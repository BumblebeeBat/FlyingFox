-module(packer).
-export([pack/1,unpack/1,test/0]).
-record(p, {p = ""}).
unpack(JSON) ->
    J = jiffy:decode(JSON),
    if
        is_list(J) and (hd(J)==<<"tup">>) -> 
            R = list_to_tuple(tl(J));
        is_list(J) and (hd(J)==<<"rec">>) -> 
            R = list_to_tuple(tl(J)),
            G = binary_to_list(element(1, R)),
            setelement(1, R, list_to_atom(G));
        true -> J
    end.
pack(X) ->
    B = is_record(X, p),
    Out = if
        B -> [rec|tuple_to_list(X)];
        is_tuple(X) -> ["tup"|tuple_to_list(X)];
        true -> X
    end,
    jiffy:encode(Out).
test() -> 
    123 = unpack(pack(123)),
    [["abc"]] = unpack(pack([["abc"]])),
    #p{p=123} = unpack(pack(#p{p=123})),
    {"", 1234} = unpack(pack({"", 1234})),
    "success".
