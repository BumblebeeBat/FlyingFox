Colored lightning means having different colors of lightning channel networks on the same blockchain.
So if you start with coins, you can convert some of them to red coins. coins can be sent through channels, and red coins can be sent through red channels. Then later you can convert the red coins back into regular coins.

So if you want a dapp that involves 2 colors of coins, then people will need to have at least one of each color of channel.

Colored Lightning allow us to move a greater variety of computation off-chain into the state-channels. Specifically, it lets us move prediction market judgement off-chain.

It is also a lot more affordable than the existing mechanism to do judgement on-chain, [as explained here](cheaper_alternative_to_reputation_truthcoin.md)

Colored channels solves a problem inherent to channels. If there are >2 nodes in a channel network dapp, it isn't possible to enforce that money gets deleted in a channel you don't control. So in a simple example, if Bob, Charlie, and Alice had channels in a triangle, Bob couldn't be sure that Alice and Charlie are actually deleting money in their channel.
Alice and Charlie might be splitting the money they are supposed to delete, and keeping it.

This is an important problem for oracles. If the participants in an oracle should do a 51% attack and steal the gamblers' money, we need a way to make sure the reputation from the reporters becomes worthless.
If the reporters use a single color of channels to be the rep, then the validators can censor the tx that converts the colored coins back to regular coins, which makes the reputation worthless.

In this way, we can be sure that money gets deleted. So it is possible to make cryptoeconomically secure oracles in the lightning network.