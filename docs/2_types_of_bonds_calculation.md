Tendermint is a blockchain with only one type of bond. Lets analyse it's efficiency in comparison to Flying Fox's 2 types of bonds.

about 1/20th of tendermint's value will be locked up in bonds ready to slash, about 5%. Or more generaly, X.

about 26/4000th of Flying Fox's value will be locked up in bonds ready to slash, about 0.65%

So Tendermint's operating costs will be about 8x higher than Flying Fox's. Or more generally, X*4000/26


The cost to attack tendermint with a soft fork is as much as buying 2/3rds of the bonds, about 4% of the coins. Or more generally, X*2/3

The cost to attack Flying Fox with a soft fork is as much as buying 2/3rds of all the money in channels, around 65% of the coins.

Tendermint's security against soft forks is about 16x lower than Flying Fox's. Or more generally 2 / 3 * 3 / 2 / X = 1 / X

The security of a blockchain is (cost to break consensus and steal coins)/(cost to maintain consensus for a day). This number is important because in general doubling the spending on security should double the cost of an attack. When we are deciding how much to invest in securing the blockchain per day, we use this number.

The relative security of 2 blockchains is security(A)/security(B). This tells us how much cheaper an alternative consensus mechanism could be at securing the same amount of money. If we invested the same amount of money into blockchain security each day, this tells us how many times bigger B's market cap could be instead of A.

The relative security of Flying Fox instead of Tendermint is 16 * 8 = 128. or X * 4000 / 26 / X = 128
For the same operating cost, Flying Fox consensus is 128 times more secure. For the same operating costs, Flying Fox could sustain a market cap that is 128 times larger.

The value of tendermint bonds doesn't matter. When we use "X" as the value, the X cancels out. Flying Fox is still 128 times more efficient.

[explanation for why 2 types of bonds is so much more efficient](2_types_of_bonds.md)