The SMPC oracle is the killer app for Flying Fox. Every aspect of the blockchain is optimized for this.

Many types of oracles have been proposed for blockchains. One of the best is from the truthcoin project http://www.truthcoin.info
This oracle has only one known flaw. The decisions that each participant in the oracle made are not secret. So it is possible to bribe the oracle participants.
SMPC is a tool to make the decisions secret. SMPC will make it so participants can't be bribed.

Vitalik explains many parts of SMPC very well.
https://blog.ethereum.org/2014/12/26/secret-sharing-daos-crypto-2-0/

This file has example code to show how SMPC works: SMPC.ex

In Flying Fox, the SMPC is made up of a single SMPC coordinator, and many SMPC participants.
Each participant makes a channel with the coordinator.

The channels record all the SMPC commitments, and none of the SMPC secrets.
The coordinator acts in the protocol in an arbitrage way. He is willing to give X money to a participant, depending on Y, as long as a different participant simultaniously commits to giving the coordinator >X money, depending on Y.
None of the participants have to trust the coordinator. The protocol is trustless.

How does the protocol simultaniously commit 2 channels to the same thing?
Lightning hashlocks.
The 2 channels don't update to the new state until a hashlock secret is revealed.
The same hashlock secret updates both channels at once.


Vitalik talking about SMPC in a racent blogpost:

https://blog.ethereum.org/2016/01/15/privacy-on-the-blockchain/

"""
The requirement of trust on the participants is also an onerous one; note that, as is the case with many other applications, the participants have the ability to save the data and then collude to uncover at any future point in history. Additionally, it is impossible to tell that they have done this, and so it is impossible to incentivize the participants to maintain the system’s privacy; for this reason, secure multi-party computation is arguably much more suited to private blockchains, where incentives can come from outside the protocol, than public chains.
"""

This is a serious limitation of Flying Fox.
We need the SMPC encrypters to be incentivized to keep their secrets secret, even though

