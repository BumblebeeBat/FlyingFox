New transaction type, similar to slasher.
Each validator needs a minimum amount of money in their account.
If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
Otherwise it would be possible to reduce the total number of validators without paying a fee.

new transaction type, similar to the one above.
If an account has a very low amount of money, (low enough that their partner already closed all their channels), Then it is possible to delete the account, and take some of the money in it.

Add third type for consensus_flag in channel_block, for people who ran out of money and can't validate any more, or for people who want channels without the responsibility of delegating.

add another type of transaction for closing channels. This one should allow you to instantly close the channel, and take all the money, but only if your partner is very low on funds, and will soon lose his account.

reveal tx type

slasher tx type

besides channels and accounts, keep track of how much money has been deleted.
Pay validators from the pool of deleted money, this ensures that we don't print money.
