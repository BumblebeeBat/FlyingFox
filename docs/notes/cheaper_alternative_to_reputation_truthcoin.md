The limitation with reputation is that the total amount of money in the markets has to be worth less than how much the reputation is worth. So, if we want to occasionally have high volume markets, we need the reputation to occasionally be worth a lot. Keeping money in reputation is expensive by the interest rate. If your money is invested in rep, you can't also invest it elsewhere. So ideally we would want to decrease the value of rep sometimes, when there aren't any markets to judge over.

This paper improves on Bitcoin Hivemind's and augur's protocols by reducing the amount of money that needs to be locked up. The amount of money locked in safety deposits only ever needs be as big as the current amount of value being judged over in the moment.

To form a oracle, a group of people transform the native currency into a subcurency. They have an agreement with the blockchain, so that after X number of blocks have passed they will be allowed to transform the subcurrency back into native currency, as long as the validators let them include their withdrawl into a block.
They should have a limit to how big of a fee they can pay, whichever is less: 1% of how much money they have in the subcurrency, or the median tx fee over the last X blocks. We don't want them to be able to bribe the validators into letting them get away without full punishment. Hopefully the validator's only incentive will be the value of the currency.
This way we can quickly increase the value of reputation when we need it, and unlock the money for other purposes when we don't.
It also makes it easy for the blockchain to know the relative value of reputation and internal currency.
This makes it possible for us to give the money from lying validators to the gamblers.

For every type of money, we allow a type of channel for using that money. If you want to make a dapp that uses multiple types of money, then you use hashlocks to connect the channels together.


In Augur there are coins called "reputation". The maximum amount of money being gambled in the markets at one time must be < the total value of all the rep. This makes the cost of using augur or making markets in augur very high.


Using the simplification, every market has a seperate safety deposit stopping the judges from double-signing. The subcurrencies used to make the deposit only needs to exist for long enough for all the reporters to report. So this is much more affordable than augur/hivemind's reputation.


generalizing this concept:

It is bad to reuse bonds to different things. Every judgment needs a seperate safety deposit. We shouldn't use a large pool of reputation currency for all the different judgments.

If you reuse the same money for events that don't occur at the same time, then the money needs to be locked up a long time, this is expensive. By making each bond only do one thing, we can unlock it as quickly as possible, which is more affordable.


about market size:

It is better to have too many small markets instead of any market that is too big.
You might think that the problem with too many small markets is that there are traders getting arbitrage between the markets, absorbing value from traders. But, can use hashlocking so that gamblers are simultaniously doing the same trade in a bunch of parallel markets at once. Keeping each market small, and protecting the customers from arbitrage.
The problem with big markets is that we would need such a massive amount of money in validators at the same time. It can be hard to get that much money together at once. It is more efficient to close a bunch of tiny markets one at a time.