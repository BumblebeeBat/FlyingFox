Comparison of failure modes between flying fox consensus and BFT traditional byzantine fault tolerant consensus (like tendermint or hyperledger).


There are 3 failure modes I examine that can happen to proof of stake blockchains: bribe, forking, and halting. 
Bribe failure mode is when some change of consensus state is highly contested, and coin-holders pay money to redo history, possibly repeatedly. 
Forking is when there are more than one valid looking histories existing at the same time. Maybe half of the network follows one, and half follows the other. 
Halting is when a large percentage of validators disappear, and it becomes impossible to continue the blockchain.

Halting in BFT systems happens if more than 1/3rd of the validators go offline. This is because creating a block requires 2/3 of signatures. If this happens then the programmers are supposed to launch a new version of the software, and to take power away from the people who disappeared. In Tendermint validators who fail to sign are erased from the validator list fairly quickly, so this failure mode can only happen if an attacker buys 1/3 of the stake.

*Jumping a gap- In flying fox consensus, there is a basefee for creating the next block. Or you can skip some blocks to a higher height, putting 105 on top of 99 for example, but you have to pay much more to skip blocks. basefee*2^(blocks skipped)

Halting in flying fox consensus happens when more than 1/2 of the validators go offline. This is because 1/2 of validators will control more than 2/3 of one in 26 blocks, which is the largest gap that can be jumped. Larger gaps cannot be jumped because there isn't enough money in existence. So they just barely cannot afford to keep the chain alive. People who fail to sign lose their money with a half-life, so this failure mode can only happen if an attacker buys 1/2 of the stake. I talk about the exchange rate in the file exchange_rate.md

Forking in BFT systems happens when at least 1/3 of the validators double sign. To cure this failure mode, the programmers would launch a new version of the software where the double-signers had no stake.

Forking in flying fox happens when more than 1/2 of the validator stake is controlled by people who want to maintain the fork. This is because 1/2 of validator stake will control more than 1 in 26 blocks, which is the largest gap than can be jumped. So they can just barely keep the fork alive.

Bribe failure in BFT systems. Tendermint requires each validator to keep timestamps and evidence from their communications with the other nodes. The evidence can later be used to discover who was byzantine. Since the programmers are the ones who create the new protocol after failures, probably the programmers would be the ones being bribed. This situation needs to be experimented on.

Bribe failures in flying fox never happen because the people who disagree can use the jumping a gap mechanism to fight. If 2 people want contradictory valid state-changes, then they take turns undoing each other's transactions until the cost exceeds the benefit. The money that would have gone to bribe someone instead gets burned.

I talk more about flying foxes 1/2 vs BFT 1/3 in the file security.py

There are 2 types of bribe failures. I discuss them in more detail [here](tendermint01.md) and [here](tendermint04.md)

There is another type of attack called a "soft fork attack" that I talk about (here)[tendermint02.md]