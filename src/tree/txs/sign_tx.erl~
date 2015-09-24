-module(hashmath).
-export([htoi/1, itoh/1, test/0]).
htoi(H) -> << I:256 >> = H, I.
itoh(I) -> << I:256 >>.
test() ->
    H = hash:doit(1),
    H = itoh(htoi(H)),
    success.
