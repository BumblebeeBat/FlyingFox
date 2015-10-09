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
-define(ConsensusBytePrice, initial_coins() div max_size()).
consensus_byte_price() -> ?ConsensusBytePrice.
max_address() -> max_size() div 10 div 75.%use about 10% of space to store addresses. Each one is 75 bytes
max_channel() -> max_size() div 10 div 9.%use about 10% of space to store addresses. Each one is 9 bytes
create_channel_fee() -> consensus_byte_price() * 9.
create_account_fee() -> consensus_byte_price() * 75.
delete_account_reward() -> create_account_fee() * 3 div 4. % 3/4th refund.
security_ratio() -> 1.5.
-define(SecurityBondsPerWinner, initial_coins() div 100 div finality() div minimum_validators_per_block()).
security_bonds_per_winner() -> ?SecurityBondsPerWinner.% so around 1% of money is locked up at a time, and it takes around 4000 blocks to move all the money. %this money goes from validators, to themselves.
-define(AccountFee, initial_coins() div max_address() div finality() div 10).%so if all accounts are full, it takes 10 finalities until most of them start losing so much money that their accounts open up.
account_fee() -> ?AccountFee. 
-define(DelegationFee, 1 / finality() / 1000).
delegation_fee() -> ?DelegationFee.
%(All the money in channels, times this fee) is the amount of money that transfers from delegates who were not elected to delegates who are elected in each block, and gets locked up for finality() blocks. If this number is too high, then poor people can't afford to be validators. If this number is too low, then rich people can't move their money quickly enough.


%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.

