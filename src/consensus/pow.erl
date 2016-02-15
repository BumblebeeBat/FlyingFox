-module(pow).

doit(W, C) ->
    X = inv_pow(W/C, 1),
    NewC = ((49 * C) + W) / 50,
    {NewC, X}.
inv_pow(W, C) ->
    W/(W+C).
%C is enough to buy 1/4 of D
