New things are possible for channels:

It is possible for participants in a channel to win each other's money without publishing anything to the blockchain. This allows for SMPC and Market Makers. So we can make from the Truthcoin project.

Poker is a popular example of the new type of contract that is possible. 


How do you decide if it is cost-effective to use side-channel techniques for your contract?

N = The number of participants in your contract times the maximum amount of money a participant could recieve from the contract.

If you don't use sidechains, then you will need a loan for at least N amount of money for the entire diration of the contract. This can be cost prohibitive for some applications.

If you do use sidechain, then you will need to publish 2 transactions onto the blockchain. One at the beginning of te contract, and one at the end. This can be cost prohibitive for some applications.

Depending on the current cost of getting a loan, and the cost of publishing to the blockchain, your optimal type may switch over time.

Channels are more simple, and explained elsewhere in the docs. I am focusing on side chains here.

imagining a game with 9 players. Each player makes a channel with a central node. They agree upon a judge.

So long as none of the players have modified the software, and the central node uses normal software, they will never actually need the judge. The computer will only let you play normally.

At the beginning, you all sign a transaction explaining who is betting, where the money comes from, and who the central node is.

You are really only transacting with the central node, not your peers. So if you and the node disagree on your current balance, then the node will refuse to sign the next channel-state.
If the node is lying to hold your money ransom, then you can get your channel block signed by the pre-agreed upon judge. The judge writes down how much money each of you would get if the game were to end right now.

If the node still refuses to sign the next channel block, then you publish the channel block the judge had signed to the blockchain.

Attack: What if the judge signs in conflicting ways on each channel to steal all the node's money?
It can't happen because of how the conserved quantities works. When the first player leaves the game, their channel block explains how much money is left in the pot.
Now when other player's leave the game, their is left money left that they could possibly take. The extra-big channel block costs a larger than normal fee.

Attack: Can't the first player to leave just take all the money?
No. Everyone has to sign their own channel. The first player to leave can only take as much money as he fairly won while playing. Other channel blocks are invalid, because they don't obey the sidechain creation tx.

Instead of programming in a turing complete language like EVM, all we need is a simple language for describing conserved quantities between groups of addresses.

The goal of channels is to be able to play a poker game, and the final result of the game is our balances are different, and nothing else changed.
The way I described it so far, the game would end with a significant change to everyone's channel state. Each channel still has a conserved quantity in it explaining how money is being teleported between the different channels. It would be computationally expensive to maintain these links.

So we need a way to clean up this memory leak.

For example: if my channel says on the blockchain that it contains 100 coins, but because I won a prize, it actually contains 1000 coins. Then the channel-state would have pointers to the other 9 player-who-lost-to-me's channels.

The central node needs to clean up the memory leak to keep it's customers. It is the only one that is aware of all the channel-states at once, so it is the only one who is capable of cleaning the leak.

One way to clean it up it could send a single to_channel transaction to add 900 coins to my channel. Then it could remove the channel-links from all the channels safely.

Another more efficient option would be to find someone else who has a channel with me or wants to make a channel with me, and build a loop to simultaniously clean up my game while increasing the connectivity of the network, so that next time I play we can clean the game for free. 2 birds with 1 transaction.

We could let the memory leaks pile up for a while before cleaning them. Like periodically weeding a graden. Periodic weeding might make more sense, as we can wait for more investors with liquidity show up and complete loops. Or we can launch more servers to complete loops.