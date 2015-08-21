I use a lot of Vitalik's ideas.

From this essay: https://blog.ethereum.org/2014/01/15/slasher-a-punitive-proof-of-stake-algorithm/

1) his algorithm for selecting signers from the coin-holders, with a small adjustment. 

2) the punitive transaction type. 

https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/

1) weak subjectivity 

https://blog.ethereum.org/2014/07/05/stake/

1) the low influence random number generator, with a modification. Signers first reveal Hash(bit+salt) and later reveal bit+salt when they collect their reward.


I use something similar to Daniel Larimer's transactions as proof of stake. Every transaction must reference the hash of one of the 10 most recent blocks. That way forks that start more than 10 blocks ago wont have any tx fees to reward the block creator.#removed in current version, may re-include this feature again later.

https://blog.ethereum.org/2014/10/03/slasher-ghost-developments-proof-stake/

1) creating a new block should cost a large fee. A negative block reward. 

2) I use something similar to his idea (7), quote: "If there is an insufficient number of signers to sign at a particular block height h, a miner can produce a block with height h+1 directly on top of the block with height h-1 by mining at an 8x higher difficulty (to incentivize this, but still make it less attractive than trying to create a normal block, there is a 6x higher reward). "
But instead of charging POW, I charge a fee. 

http://vitalik.ca/ethereum/patricia.html
The patritia tree may be added at some point to help light nodes.

Ethan Buchman's idea:
The total amount of money spent in a block must be less than or equal to 1/3 of the total amount of safety deposits left by the people who signed on that block. That way, any double-spend attack ends up costing more money than can be stolen. 

Jae Kwon realized that I needed to save a tree of blocks instead of a chain. He also showed me how to do ephemeral encryption.

from Joseph Poon http://lightning.network I take the idea of channels.

from truthcoin https://github.com/psztorc/Truthcoin I take the idea of oracles, I may take the oracle mechanism too. I plan on using CFD contract for difference instead of LMSR, because CFD can fit into a channel. from http://www.truthcoin.info/blog/pow-cheapest/ I realized that minimizing the amount of bonded money is so important.


From a mailing list, forgot to write his name down. I didn't add this feature yet, but I may in the future.
minority game device to generate random-like numbers-
is a simple anti-consensus game. At each round, M players can bet stake S on 1 or 0, and those who bet with the majority lose their stake and pay those who bet with the minority, who win > 0.5 * S * M. Idea is that you generate an N bit number by running N iterations of the game (i-th bit = minority answer in the i-th round). So you have an economic argument that numbers will be random-like because if P(i+1 = 0 | {1,2,...,i}) didn't equal 0.5, there would be a profitable strategy to play in the minority game. 