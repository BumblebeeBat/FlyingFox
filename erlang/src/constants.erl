-module(constants).
-export([initial_coins/0,finality/0,validators_elected_per_block/0,minimum_validators_per_block/0,master_pub/0]).
initial_coins() -> round(math:pow(2, 48)) - 1.
finality() -> 26.%/docs/security.py explains why.
validators_elected_per_block() -> 54.
minimum_validators_per_block() -> 36.
master_pub() -> <<"BM1PdUiu7IaSzAH0rXgCR6/RYP0x3EJ8gnd1bCoNfr0xSRkalnb0xhWmjF24K9z0GtnCAlh0A6brk1jC1WlI8gs=">>.
%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.

