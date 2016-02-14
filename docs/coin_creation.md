It is bad to connect the consensus mechanism to the coin creation mechanism, because the rate of coin creation should be determined by the demand for new coin.

When the demand for gold goes up, we invest more into mining, and rate of gold production increases to meet demand.
When the demand for bitcoin goes up, we invest more into mining, and the rate of coin production stays static.

Having a fixed unchanging rate of supply is bad. It makes the currency price very volatile.
What we really want is for it to always cost 1000 coins of POW to produce 1000 cryptocoins.

If we disconnect the consensus from coin creation, then we can create something much more elegant. We can sell small amount of coins for small amounts of POW to determine the current exchange rate between coins and POW, then offer to sell any amount of coins at the current exchange rate. The resulting blockchain would be able to grow it's market cap very quickly when it needs to, without changing the price of individual coins.

We need a tool to measure the current POW price. Then we can sell as many coins as they want at that price.
To calculate the price, the block creator should have the ability to earn back a portion of the coins he burns D.
The block creator should choose a number B between 0 and 1. The bigger the B, the more work he has to do.
At 0, he gets none of his money back, at 1 he gets it all back. like B*D
The closer he is to 1, the more POW mining he has to do exponentially. Like C/(1-B)
The optimal strategy of the block creator is to select B such that his reward is big, but the amount of work he has to do is small.

I drew a greph and did this math in pen, check coin_creation.jpg
If the block creator selects B as profitably as possible, then the price of a hash in coins is (1+B)/(1-B).
