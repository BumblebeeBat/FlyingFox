== What if a second oracle promises to output the same as the first oracle?
You have to pay the second oracle's fee, and trust that the second oracle wont lie.
This will always happen, because different people will prefer to trust different oracles. If you know the participants of an oracle, or you know that they have a big loan in a bank in gold or land or fiat, then you may choose to trust them differently. You may decide to only trust oracles that have the stamp of approval of 3-letter government agencies.

== Who pays the oracles?

Usually, oracles will judge bets off-chain.
The oracle can choose to charge any fee to provide judgment, but once it has provided judgement once, it's judgement can be copy-pasted into all the channel-blocks. So they have judged over everything at once.

We can use signature-lock bets to combine many people's small payments, so that when the oracle judges on the outcome, it simultaniously unlocks all the payments to pay the oracle. None of this gets written to the blockchain.

There is still a problem: the gamblers don't have an insentive to participate in paying the oracle.
Solution: Use a truthcoin dominant assurance contract (TDAC) to raise the money.

Since this depends on the amount of money raised, which is totally mechanical, we aren't depending on another oracle circularly. 

Problem: I need to prove that it is possible to make the TDAC contract short enough that it could be published to the blockchain.
If Alice has Channels with Bob, Charlie, and Dave, then Alice could participate in the TDAC with volume of Bob's participation, plus Chalie's, plus Dave's. Bob, Charlie, and Dave's pubkeys wouldn't have to be written in the TDAC. Alice's extra big bet is hedged by using the lightening network to let B, C, and D to bet through her.
Therefore, it is possible to combine all the bets through the lightening network so the TDAC contract is short enough to publish to the blockchain.

