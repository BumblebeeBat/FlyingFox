We need a mechanism to measure the current POW price. Then we can sell as many coins as they want at that price.

We need to disconnect the consensus from coin creation. We can sell small amount of coins for small amounts of POW to the block creator to determine the current exchange rate between coins and POW, then offer to sell any amount of coins at the current exchange rate.

Steps of the mechanism:
The block creator should choose a number x between 0 and 1. The bigger the B, the more work he has to do (w).
R = A+(B/2) is the money that is usually destroyed to create a block. The block creator can win some of it back by providing POW=w.
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
