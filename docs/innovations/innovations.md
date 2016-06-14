[on youtube](https://www.youtube.com/watch?v=sEGyfg7AB0Q)

Flying Fox is a cryptocurrency with Turing complete smart contracts.
Here are 5 innovations written for Flying Fox that no other blockchain has.

1) Flying Fox uses [the lightning network](docs/lightning_network.md) to instantly spend money, and to instantly deploy smart contracts. You don't have to wait any confirmations.

2) Flying Fox achieves the "useful proof of work" goal. The value that gets destroyed maintaining consensus is simultaniously being used to maintain the lightning network. Lightning networks can be expensive because a lot of money is locked up, this cost is passed to the users in the form of transaction fees and rents. The blockchain consensus protocol used for Flying Fox is called ["Lightning Consensus"](docs/lightning_consensus.md). Lightning Consensus covers some or all of the lightning network cost, so you can send money with lower or zero fees, and your rent cost is lower or negative.

3) There are 2 plans for adding scalability to smart-contract blockchains. Either state-channels, or shards. Flying Fox is the first implementation of state channel blockchain scaling. State Channels require less assumptions than sharding. Sharding is experimental, we don't yet know how to build it. Channels are simple and understood.

4) Flying Fox has a finite memory requirement, old blocks are deleted. For every million users, the entire blockchain is less than 100 megabytes.

5) Flying Fox is more affordable than any competing POW or POS consensus because it uses [2 seperate types of bonds](docs/2_types_of_bonds.md)