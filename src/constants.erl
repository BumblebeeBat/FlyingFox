-module(constants).
-compile(export_all).
%-export([export_all]).
initial_coins() -> round(math:pow(2, 48)) - 1.
initial_delegation() -> 1.
finality() -> 26.%/docs/security.py explains why.
validators_elected_per_block() -> 54.
minimum_validators_per_block() -> 36.
chances_per_address() -> 200. %Each address has this many chances to be a validator per block. this makes it possible to be validator more than once on the same block. 
master_pub() -> <<"BEuR+aFDJUclPTzlGWU1zWoSph9Zos7qPqOza4VsQ5B1gp8lT5gdzvFaX/sFzdz1Sy2vfKaCvaubVjjGpPDenLo=">>.
max_size() -> 200000000000.%should be 200 gigabytes, does not include old blocks.
consensus_byte_price() -> initial_coins() div max_size().
max_address() -> max_size() div 10 div 75.%use about 10% of space to store addresses. Each one is 75 bytes
max_channel() -> max_size() div 10 div 8.%use about 10% of space to store addresses. Each one is 75 bytes
create_channel_fee() -> consensus_byte_price() * 75.
create_account_fee() -> consensus_byte_price() * 75.
delete_account_reward() -> create_account_fee() * 3 div 4. % 3/4th refund.

%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.

