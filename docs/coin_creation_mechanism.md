We need a mechanism to measure the current POW price. Then we can sell many coins at that price.
It is made of 2 smaller mechanisms.
The first measures how much higher the price could be if we change the supply being sold. Looking at the SupplyDemand.png, this first mechanism is useful when we are to far to the left and we need to increase price.
We are estimate a price function that is dependent on how many coins we sell. A supply/demand function.

Over very short time periods, we sell finite numbers of contracts giving people ability to mine a set number of coins at a set price. Based on the fees people are willing to pay for the priviledge, we can estimate the price function.

Once we know this function, we can choose the right price and right amount of shares to sell to balance our 2 goals: Growing the market cap, and increasing the price of individual shares.

This second mechanism can measure if the current price of coins is too high. This mechanism is useful when we are too far to the right, and we need to lower the price.

We can sell small amount of coins for small amounts of POW to the block creator to determine the current exchange rate between coins and POW.

Steps of the mechanism:
The block creator should choose a number x between 0 and 1. The bigger the B, the more work he has to do (w).
R = A+(B/2) is the money that is usually destroyed to create a block. The block creator can win some of it back by providing POW=w.
B = Beta*A. Beta is about 5%, that so the block creator has a 10% range of prices to choose between.
By making B smaller we can increase the monetary incentive for the stimate of price to be accurate.
By making B bigger we can increase the range of prices that the block creator can choose between.
The amount of work he has to do depends on x like this: work(x) = (A*x)+((B*x*x)/2).
His reward depends on x like this: reward(x) = R*x

Now to estimate the new difficulty from x:
The block creator wants to earn as many coins as possible, as long as the work he does costs less than or equal to the reward.
So he will choose x where these 2 lines cross:
d/dx work(x) = reward(x) 
-> d/dx (A*x+B*x*x/2) = d/dx(reward(x)) 
-> A+Bx = R
QED new_difficulty(x) = (A+(B*x))/R hashes/coins

This calculates a minimum for the new difficulty. If we are selling large numbers of coins at a lower price, then the block creator probably wont be willing to pay a higher price now.