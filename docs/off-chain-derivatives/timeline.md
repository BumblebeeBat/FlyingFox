




Alice makes a megachannel for gambling.
Many people join the megachannel, and make bets against Alice.
Some of the customers disagree on the outcome of the bet.

Alice judges the bets that are disagreed, the act of judging does:
* if Alice lost, she pays her partner and we are done.

If Alice won, then:
* Alice publishes a tx on-chain with evidence from the channel, this changes the channel into a new contract only involving Alice, and not her partner.
* the money is locked in this contract for a time so that the distribution of coin-holders is able to destroy the locked money, if they think Alice lied.
* Publishing the tx on-chain involves revealing a hash. Revealing this hash simultaniously causes


* the contract says it has a small limited time to be published, N blocks in the future.
* it has an id. it is only possible to publish if this id isn't blacklisted.


* The money that was in the bet is locked down for N blocks.
* if the money isn't destroyed, Alice eventually gets it.


