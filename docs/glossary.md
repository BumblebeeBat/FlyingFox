Words useful for talking about FlyingFox

oracle - one or more users form a team called an oracle. Decisions are submitted to the oracle

decision - A yes/no question submitted to an oracle. The oracle decides the outcome of the decision.

cash - the basic form of currency in FlyingFox. Cash is an asset, but not a liability. It can be spent at any time. 

bonds - When validators sign over the next valid block, some of their money becomes temporarily unaccessable. If the validator tries to cheat by signing another block at the same height, then their money is taken away.

blockchain - a method of maintaining consensus over some data. 

consensus state - The data which is identical on every node. 

block - Used to update consensus state. Blocks contain multiple transactions. A block must conform to many rules to be considered valid.

transaction - Explains how the consensus state should be changed. Creating transactions is how users interact with the blockchain. This is how to spend money and make bets.

channel - a channel is like a small blockchain for 2 people. It allows users to use the blockchain without paying any fees. 

channel state - a contract explaining who owns which parts of the money in a channel. If both participants in the channel work together, they can update to a new channel state.

mechanism - this is the thing that is being bet on. It is a true or false statment. It might be simple, like "Obama wins the 2012 election." or complicated source-code for a computer language. The computer program must return True or False.

validator - On every block a random sample of bond holders is selected. This group of bond holders has the privelege of selecting the next block for the blockchain. 

contract for difference - betting in flying fox uses contract for difference (CFD). It is a type of betting that happens between pairs of people. The pair of people promise to trade an amount of money based upon some future price. It is useful for hedging your risk, for example if you own 3 tons of onions and want to be protected from the possibility of onion price dropping.

sidechain - a channel that has more than 2 participants (we probably wont use this)

channel link - channels that are linked together can have money flow between them. If you don't unlink channels before broadcasting them, then you have to pay an extra fee. Unlinking channels requires putting enough money into each channel so that all the channels have non-negative amounts of money in them. Channel loops can help make each channel non-negative. This complicated feature might not get created.

channel loop - example. if Alice has a channel with Bob, and Bob has a channel with Charlie, and Charlie has a channel with Alice, then they are in a 3 node channel loop. Channel loops are convenient because they can provide extra liquidity.
If Alice wants to play poker with Bob, but doens't have any money in the channel with Bob, she can still use money from her channel with Charlie to play poker.
Alice sends charlie money in their channel, and charlie simultaniously sends bob money in their channel, and bob simultaniously sends Alice money in their channel. It all cancels out so no one gains or loses anything.
Now Alice has enough money in her channel to gamble with Bob, and no one had to publish anything to the blockchain.

block height - This is how many blocks there are between the genesis, and the top of the block tree. Blocks are looked up from the database by height. 

block number - This number starts at 0, and usually increments by 1 per block. This number determines who the signers are for the block. If insufficient signers can be found for number N, then the signers for block N+1 can go first, but it costs more exponentially. In the situation where a block number is skipped, the height is NOT skipped. So the block number starts equal to block height at 0, and block number grows equal to or faster than height.

cryptoeconomic security - A mechanism is considered cryptoeconomically secure against attack X when we can prove that the cost of performing attack X exceeds the amount of money that can be stolen or destroyed by attack X.