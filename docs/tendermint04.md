Tendermint is unprepared for double-spend attacks

A double-spend attack is when the validators re-make one or more blocks to undo payments. The cost of an attack like this is at least 1/3rd of the security deposits from the validators. The maximum profit from an attack like this is the volume of money that was transfered in the block.

The only known mechanism to be secure from this type of attack works like this:
1) The amount of money spent per block needs to be less than 1/3rd of the security deposits protecting the block.
2) security deposits cannot be re-used for different blocks. Each block needs a seperate security deposit that lasts until finality.

Tendermint has set their finality to 1, so it is impossible for them to be secure against an attack like this. They claim that they will use a hard fork to recover, but Hard forks can't necessarily recover from this failure mode because:

We would need decentralized process to decide which state we are going to hard fork to. This decentralized process would need to be resistant to forking.
Any decentralized process resistant to forks that can come to consensus about state is called a "blockchain".
So, for Tendermint to recover using a hard fork, it would have to be dependent on another blockchain like bitcoin.
If the bitcoin miners choose the side that harms the majority of Tendermint users, then a majority of Tendermint users will do a hard fork to keep the other side alive. Both sides would live, which is a failure mode. So we can't depend on another blockchain either.
