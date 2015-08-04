Linking channels

Why would you want to link channels? To make SMPC, and market makers, LMSR, poker

Theoretically, someday it will be possible to make a more advanced form of linking channels where some participants are like the validator for a new blockchain. This is called "sidechains". Here I am prosposing 2 more limited alternatives which will exist in the nearer future. These 2 strategies only use 2-person channels.

Like all channels, so long as none of the players have modified the software, and the central node uses normal software, they will never actually need the judge. The computer will only let you play normally.

When linking channels, you have to choose between 2 strategies. One strategy involved publishing 2 tx to the blockchain. The other strategy involves locking up a lot of liquidity for the duration of the contract. I will compare their strengths and weaknesses here.

Limitations/Advateges for the strategy where we need to lock up liquidity:
* If we add more people to the linked channel, then we need to increase the amount of locked liquidity.
* We never have to publish at tx.
* The amount of liquidity is the sum of how much money each person in the channel could possibly win in a sigle channel block.


Limitations/Advantages for The strategy where you have to publish 2 tx:
*You only have as much money as you start with in the channel, unless you publish a 3rd tx to the blockchain to exaplain who is getting money and in how much.




imagining a game with 9 players. Each player makes a channel with a central node. They agree upon a judge.

Attack: what if the judge refuses to take yout your money?
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

attack: what if the central node know that you have a high probability of winning, so he blocks your communication to boot you from the game?
Give all the information to the judge. He will decide what is most fair. Pick a judge who has a history of fair decisions. 
