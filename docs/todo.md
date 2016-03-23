remove fee from record channel_block. It does nothing.

We need to add a fee to sign_tx. This fee can possibly be negative.

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

It is inefficient to store things in base64 mode. keep it decoded as much as possible.

Maybe I should start out owning $1,000,000 of coins or so, otherwise it will be too cheap for a whale to print a bunch of coins and refuse to take part in consensus. The $1,000,000 of coins could be unspendable, only for consensus purposes.

It would be nice if we have a javascript interface to send spam-less messages. The recipient has the option of deleting the sender's funds.

close_channel javascript multiple steps
*Look in channel_partner. Make sure our partner has = or < nonce than channel_manager.
change server javascript
delete_account javascript
button to rotate through your inbox

Make an OTP erts package so that it is easier to install.

handler should have every input and output be encrypted. Otherwise eavesdroppers will publish our channel before we want it published.

We need tests to make sure that skipping a height works. It shouldcost more for the block creator.

constants:security_bonds_per_winner() should be tuned. The random number generator should be seeded from a long enough time ago.
We want it to be impossible to cause a fork by bribing validators to double-sign. There shouldn't be enough money in the blockchain to maintain the fork long enough to have different randomness on each side.
Block tree needs to hold many more blocks. Lets try to keep it below 200 megabytes of ram or so. We need to know everyone's balance long enough ago because the random seed is from a long time ago. We want it to cost >50% of the money in the blockchain to cause a fork. (maybe we don't need to save the whole block. We only need to remember the random entropy and everyone's balances.)


the idea was introduced here: https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/
Weak subjectivity is necessary for security reasons. It stops long range attacks.
POW solves the same problem, so long as people are still mining.

Add onion routing to messaging

https://blog.ethereum.org/2016/02/17/smart-contracts-courts-not-smart-judges/
Maybe upgrade the off-chain code so that it is computed by binary search.
