Ethereum is a blockchain that allows for turing complete computation on chain. It allows you to save arbitrary data on chain, and manipulate it in arbitrary ways. There is complicated assembly-like language where every opcode has a gas fee, and the cost to run a contract is the cumulative gas consumption. Ethereum code is written in custom high-level languages that get compiled down to the opcodes. You have to pay tax to hold state on the chain.

Flying Fox allows all the same features as Ethereum, but it does this without using a turing complete language, or gas fees, or compiled languages, or arbitrary on-chain state.

In Flying Fox, data structures are saved off-chain in merkle trees. The root of the merkle tree is inserted into the channels of everyone who is participating. They all sign over the root to agree with the rules in the merkle tree.
Alice and Bob both have channels with Charlie, and are participating in Charlie's channel state. If Alice can prove that Bob broke a rule, then Alice can demand money from Charlie. Charlie uses that evidence to then demand money from Bob. So Charlie never has any risk. He is collecting arbitrage.
The entire channel state never has to get published to the blockchain. Punishing Alice only requires publishing the part of the channel state that says the rule that Alice broke.







I think "all the same features as Ethereum" is an exaggeration
basically there are lots of smart contracts that  can't actually be considered trade agreements
like, say, namereg
3 hours ago
It seems to me that it has all the same name registration abilities without as much blockchains bloat. Since you don't have to store hashes of names in blockchains state. All the name hashes are off chain.
the point though of namereg is that everyone looks at the same place and is guaranteed to see all the registrations
You can have that style of namereg off chain too
so the bloat will be there anyways, only you won't be able to prove that it's censorship resistant
I don't count it as bloat if it isn't in the blockchain state.
The only people who have to download the data are people who want to know about name registrations
I don't see how it lacks censorship resistance
well you have to guarantee censorship resistance to have it
Removing the ability to have arbitrary on-chain data doesn't seem to break the existing guarantees about censorship resistance.
sure it does - unless you post a state hash of the namereg contract on-chain, at every block
actually posting the hash only convinced me that you don't have two versions of the namereg
It doesnt convince me that you'll show me any registration if i ask for it
How would you convince the participants of the off-chain namereg to be silent about the current channel state?

Info wants to be free, as the teams get larger it gets harder to convince the participants of secrecy.
sure yeah what you're doing now is trying to provide the censorship resistance guarantee
If 2 participants revealed the channel state, and one has a higher nonce, then you can tell who lied.
If I made a channel with you that had state like:
1) there is a list of validators
2) a name I claim
3) the highest nonced channel state root
4) a merkle proof showing my name from the root.

If you can get a higher nonced state that the validators signed, you could channel-slash me and take my money. Once the time limit is reached you can't slash me any more, and it is possible that the namereg state can be updated again. 

You can private message with any of the validators.
You could make a channel with other validators offering a reward in exchange for the data you need to slash me.

If you can't find the recent channel state, then you can't slash. 

It is similar to using ethereum, in that you can't be sure you have the most recent block yet.
If you are 1 block behind everyone else, someone could trick you into thinking they have a name registered to them.
The problem of finding a recent hash to sync with the blockchain is the same as the problem of finding the most recent hash to sync up some off-chain state.
seems like you're making a chain and calling it a channel because the deposits are on a(nother) chain
do the validators need consensus?
Each channel could have a slightly different list or weightings for the validators.
People doing arbitrage will demand identical lists on both sides of course
could also be said of chains - what are you calling arbitrage, here?
The incentive will probably be to update your channel validator set to conform every time the off chain validator set changes
Arbitrage is collected by Payment hubs that trustlessly do hashlocked Lightning transactions
I don't think it is a chain. The properties are different.
I think it is a bunch of hashlocked contracts in channels.