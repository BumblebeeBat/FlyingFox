* the hub agrees to include a bet in a particular round before he knows how many or the price at which the customer gambles.
* if the hub fails to include a bet that he agreed to include, then the hub and gambler both lose money.
* bets that are matched in the same round must all be at the same price.
* the off-chain state of each round must include the hash of the round before. That way we know that the rounds are ordered in time.
* if the hub signs 2 different channel states at the same round height, then he loses everything. That way the hub can't fork the market.

