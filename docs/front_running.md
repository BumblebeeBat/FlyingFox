Lightning bets can be nearly instant. They are encrypted and only told to a single person, so only that one person could front-run you. You can bet in small increments, so the single person's ability to front-run you is minimized.

Creating a hashlocked payment uses more liquidity than a normal payment, but we can usually recover that liquidity quickly and trustlessly. If your partner wont cooperate, you may have to wait a while to unlock your channel.
Hashlocked bets are the same way. The liquidity can be recovered by the next block, if your partners cooperate. To recover liquidity, bets can trustlessly be moved from indirect channel-paths to direct channel-paths.

One way to put the market's state onto a blockchain is a matrix: things to bet on X participants who bet. This way is friendlier to market makers than order books.
Another way is store the state is by a different matrix: participants who want to bet X participants they are betting with. So you only store 2-party channels. This way only works with order books.


If the number of channels gets excessively large, they can be trustlessly reorganized so that they will require less channels to encode the same bets. Similar to defragging an NTFS on a hard drive.