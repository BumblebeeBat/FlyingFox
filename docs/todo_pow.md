Having an auction for the block creator only makes it possible to move the price down.
We need a way to push the price up too.
If we limit the number of coins that can be created, and allow arbitrary fees, then we can look at the fees that are being paid for ability to create coins. If these fees get much bigger than zero, that is an indicator that the price is too low.

If the fees that they are willing to pay exceed the amount of coins being destroyed to create a block, then the blockchain is in danger of breaking. The incentive would be to create blocks as quickly as possible, ignoring transaction fees. The coins would all be created at a steep discount.
That is a much less efficient state for the blockchain to be in. A lot more state for less fees paid. 

We want this to be true: (MaxCoinsProducedPerBlock - CostToProduceThem) < CoinsDestroyedPerBlock.

Alpha = CoinsDestroyed / MaxCoinsProduced
Alpha is a small number, just above 0. Like 0.05.

We need to make sure that the ratio FeesPaidToCreateCoins/CoinsCreated stays smaller than Alpha all the time.

So mining is split into 2 transactions. 1 sign up for a block to mine on, and a difficulty. 2 post the work that you agreed to do.
They should have maybe 30 or 100 blocks of time to complete the work in.

During the first tx take a safety deposit to incentivize them to only sign up if there is a greater than 50% chance they will be able to mine on time.

Maybe I should start out owning $1,000,000 of coins or so, otherwise it will be too cheap for a whale to print a bunch of coins and refuse to take part in consensus. The $1,000,000 of coins could be unspendable, only for consensus purposes.
