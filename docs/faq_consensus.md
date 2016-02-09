Why do we have so many validators on the same block?
We want it to be very expensive for an attacker with < 1/2 the coins to control the blockchain. If the attacker has 49% of the coins, and there is only one validator per block, then he automatically has 100% control of 49% of blocks.

The way flying fox works, you need to control at least 2/3 of a block to have complete control of the order of transactions. Otherwise the other >1/3 can work together to stop you.
If an attacker has 50% of the coins, he will only have control over 1 in 25 blocks. Those gaps are too big for him to afford to jump. So he can't have complete control over the blockchain.


What is the "heaviest chain" rule for Flying Fox?  If you wake up and see two blockchains of length 100,000, which forked awhile ago (such that each chain had a group that attempted to prevent members of the rival group from opening channels)?

In Flying Fox it is not possible for the chain to fork the way you describe. If 2 groups of validators were very determined to disagree on a particular block, it is like an auction. Whichever side is willing to throw away more money wins. It is more affordable for the side that has more validating power. The price of "raising" is at least 50% more than the previous raise. So it is a discrete process with exactly one winner. Everyone who stays online 24/7 can be certain that they are on the same chain they started with.

An attacker, instead of buying up tons of miners and wasting electricity, would be buying up lots of coins and provably destroying them. Which makes the rest of the coins more valuable. Flying Fox has anti-fragility built in. Attacking it makes it stronger.

It is possible to get a bunch of old private keys, and start building a fork from an old block.
This result is identical to taking the source code and launching a new chain from genesis block.
You treat it the same as any other altcoin. You go onto coinmarketcap.com or some exchanges to look up the exchange rate.
Either 1) you only have coins on the original chain, or 2) you have coins on both chains, and can't tell which is the original.
Either case is fine.

It cannot be profitable to make a fork by paying the jury of validators to double sign at every height.
The jury loses a safety deposit that is twice as big as the amount of money spent in the block.
The random seed is from a very long time ago. You would need >50% of the money in the blockchain to sustain the attack long enough for the random seed on each side to be different. 