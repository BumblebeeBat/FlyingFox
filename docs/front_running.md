Lightning bets can be nearly instant. They are encrypted and only told to a single person, so only that one person could front-run you. You can bet in small increments, so the single person's ability to front-run you is minimized.

Creating a hashlocked payment uses more liquidity than a normal payment, but we can usually recover that liquidity quickly and trustlessly. If your partner wont cooperate, you may have to wait a while to unlock your channel.
Hashlocked bets are the same way. The liquidity can be recovered by the next block, if your partners cooperate. To recover liquidity, bets can trustlessly be moved from indirect channel-paths to direct channel-paths.

One way to put the market's state onto a blockchain is a matrix: things to bet on X participants who bet. This way is friendlier to market makers than order books.
Another way is store the state is by a different matrix: participants who want to bet X participants they are betting with. So you only store 2-party channels. This way only works with order books.


If the number of channels gets excessively large, they can be trustlessly reorganized so that they will require less channels to encode the same bets. Similar to defragging an NTFS on a hard drive.

the batch-trading at uniform price method explained by Casey Detrio on Thursday at ethereum's devcon1. https://youtu.be/lmsOP1D8zNs?t=1h15m27s
He has contributed to this forum too.

The idea is that there are rounds of equal length of time, the rounds come at a regular frequency. When people make bets during the same round, the hub has to give them all the same price.

Different markets should have different frequencies. If you only check the markets once a day, it is in your interest to use markets with frequency of about 1 per day. Otherwise any open trades you leave are free options for traders who go online more frequently.
If all the traders can put their computers into the same building, then it will be more efficient to have the frequency be 10 per second or faster.

On-chain markets are limited by the speed of blocks. Ethereum is 12 seconds per block. It is possible to have markets with slower frequency, but not faster.

If your blockchain is the kind that forks (all POW blockchains, and Flying Fox, but not tendermint), then trades aren't finalized until they are enough confirmations deep. So on-chain trading is at risk of being front run by the miners.

Off-chain markets don't have these limitations. Trading can be as fast as sending messages. We can have markets at whatever frequency the customers want.
Hubs can self-impose rules that couldn't be imposed on miners.
It is possible to commit to promises in the channels. So if the hub breaks one of the self-imposed rules, then it loses all it's money in all the channels. All the rules can be wrapped in a merkle structure, so the proof that the hub broke a rule can be concise, even if the number of rules is very long.
Verifying proofs like this doesn't need turing completeness. Flying Fox's language is similar to bitcoin script.

There are many sets of off-chain rules that result in batch-trading at uniform price. One list of rules to achieve this is in batch_trades.md
A benefit to off-chain markets is that we can update the list of off-chain rules that powers markets without having to modify the blockchain consensus code. 