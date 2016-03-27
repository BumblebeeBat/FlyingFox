A channel has an amount of money in it. For example: 5000 coins.

Each of the two users controls zero or more coins in the channel. If one user controls 4000, then the other user must have control of 1000 or less.
For our example, to start Bob has 4000 in the channel, and Alice has 1000.

A bet is a piece of software in the channel that controls some portion of the coins in the channel. For example, if alice wanted to send 100 coins to Bob through a bet, then it would look like this:

Alice : 900
Bob : 4000
bet : 100

This is recorded between two pieces. One piece is on the blockchain, and the other piece is in a channel. On the blockchain:

Alice : 1000
Bob : 4000

The stuff on the blockchain cannot be easily changed. Stuff in the channel can be easily changed. In the channel, we need the ability to spend money. And we also need the ability to have bets that last a long time.
In the channel:

amount : 50
bet : 50

amount is the amount of money that Alice will send to Bob if the bet is split 50/50. Amount is bounded by -4000 and 1000. When Bob or Alice want to spend money, they adjust this number.
 bet is the amount of money controlled by the bet. If Bob wins the bet then we add this to amount, and Alice sends 100 to Bob. If Alice wins the bet then we subtract this from amount, so Alice sends nothing to Bob.
bet is bounded by zero below.
In order to make sure that no one can have negative money, these conditions are imposed:
bet has to be smaller than 1000 - amount.
bet has to be smaller than amount + 4000.
