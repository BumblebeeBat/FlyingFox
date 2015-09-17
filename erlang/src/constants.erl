-module(constants).
-export([initial_coins/0,finality/0,validators_elected_per_block/0,minimum_validators_per_block/0,master_pub/0,consensus_byte_price/0]).
initial_coins() -> round(math:pow(2, 48)) - 1.
finality() -> 26.%/docs/security.py explains why.
validators_elected_per_block() -> 54.
minimum_validators_per_block() -> 36.
master_pub() -> <<"BPbEUW6szoYLoRBm94Ky0rxtuoXGhhGfv6r8fteMtkl1sl4ahEIhUDrAM5h6FJuz/pjQspIkN2yh5nD15HAZEPU=">>.
max_size() -> 200000000000.%should be 200 gigabytes
consensus_byte_price() -> initial_coins() div max_size().
%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.

