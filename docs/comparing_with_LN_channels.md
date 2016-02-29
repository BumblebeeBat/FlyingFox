There are 3 major advances that FF has over LN.

Flying Fox is a blockchain optimized for channels. It uses a new proof of stake consensus mechanism optimized for channel.

---> 1) FF channels can reduce the cost of maintaining blockchain consensus, LN channels cannot.


FF channels can store arbitrary state off-chain, and you can use a scripting language to write arbitrary conditions based on the data's contents. The code in the contract is in a merkle tree, so you only publish the minimum amount necessary onto the blockchain. So we aren't constrained by how long the script can be.


----> 2) FF channels allow for arbitrary smart contracts. LN channels only allow simple bitcoin scripts.
FF channel smart contracts are not turing complete, it is similar to bitcoin script, so I don't need to keep track of gas. It is a lot simpler than ethereum, but offers the same services. None of the smart contract state is stored on-chain, so there should be a lot less blockchain bloat than ethereum.

Flying Fox channels makes it possible to link state across multiple channels. Several dozen people could make a channel with the same middle man. The middle man is collecting arbitrage as a trustless orchestrator of a Dapp.
Linking state like this is how we can use channels to build trustless things like SMPC https://blog.ethereum.org/2014/12/26/secret-sharing-daos-crypto-2-0/ or poker games.
I think that SMPC is a necessary ingredient for building truthcoin style oracles http://www.truthcoin.info/ (Paul Sztorc thinks it isn't necessary, but that it would help.)
SMPC would be too expensive on-chain, because the number of messages required for a single addition is the square of the number of participants. If we want SMPC, then we need to store state off-chain like how FF does.

FF channels allows for trustless prediction markets, crowd funding contracts, sports betting, etc. LN channels can't do any of this.

In LN, if you use multiple third party arbitrators on your channel, the size of the state you need to keep track of grows exponentially with the number of oracles you depend on. In FF the oracles could be full SMPC truthcoin oracles, instead of the less powerful M of N multisigs.

---> 3) Flying Fox state grows linearly instead of exponentially as you increase the number of things you gamble on.

