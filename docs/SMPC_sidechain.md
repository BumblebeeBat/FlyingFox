The data in an SMPC is secret, so it cannot be put onto a blockchainchain. And yet we need to maintain consensus over it in a decentralized way. So, SMPC will be a channel with more than 2 people, also called a sidechain.

Channels have a maximum size for state. It is the size of a block on the parent chain. To stop from reaching the maximum size, channels must infrequently post a list of owners of the channel, and how much control each of the owners has. Each channel must set rules for itself how much the owner set can change before they need to tell the parent chain.

Traders should be able to make LMSR side chains, and have the option to put their bets into an SMPC, so you cannot tell how the other participants are betting.

For a fee anyone can kill the sidechain and merge it's state with the main chain. If a channel-state with a higher-nonce than the kill tx is presented to the blockchain, then the blockchain will cancel killing the sidechain, and the sidechain will live on.