There are multiple blockchains. It would be nice if blockchain A could use coins from blockchain B. Pegging and synthetic assets are 2 strategies to do this. The goal of this paper is to convince you that synthetic assets are the better way.

Pegging is when you simultaniously delete coins on one blockchain, and make them appear on the other. Later you can move the coins back.
The issue with pegging is that both blockchains have to be aware of the inner working of each other. If you want to hard-fork update one blockchain, you need to update both. Each blockchain includes a light-client for the other blockchain.
If you want to be able to peg with 100 different blockchains, you would need a light-client for all of them.

Synthetic assets are made up of financial derivatives. For example, if I have a much of coins on blockchain A, and none on blockchain B.
If I use my A-coins to make a bet that B will increase in value relative to A, then the value of my bet changes as the value of B-coins change. If I made made multiple bets, I could create an asset out of A-coins which almost exactly matches the price movement of B-coins.
So if B-coins doubled in value, then my asset on the A-blockchain will also double in value.

Pegging only has 1 purpose. Moving assets to other chains.
Synthetic assets are a tool that can be used to build many different useful things.

Besides mimicing other blockchains, synthetic assets can be used to mimic apple shares, or gold, or US savings bonds.
Synthetic assets can be made to give you extra leverage. They give far more freedom that pegging.

Adding synthetic assets to a blockchain is fairly simple. You need a reliable oracle, and markets for trading.
Financial derivatives let you gamble on sporting events, and participate in crowdfunding, and learn secrets about the world with prediction markets.
Pegging doesn't have any of these features.

Pegging can break blockchain consensus. The consensus mechanism is designed to be barely secure enough to protect the coins that exists in it. If we put coins from a different blockchain, those coins change value all the time relative to native currency.
If they get to valuable, then more attacks become profitable. In the extreme it is like this:
if ethereum let people move bitcoins onto ethereum.
The price of ether drops to zero, but there are still thousands of bitcoin on the chain.
An attacker could easily buy up all the ether, or bribe the validators to confiscate the bitcoins.

Synthetic assets don't have this problem.


You could use state channels to trade A-coins on blockchain B for B-coins on blockchain A, without modifying any blockchain consensus code.

Pegging involves writing custom code for every pair of blockchains that are linked. This code is in the blockchain consensus, it is the most expensive kind of code to write.

It takes about 100x more work to write and maintain a line of consensus code vs a line of code.

