New transaction type, similar to slasher.
Each validator needs a minimum amount of money in their account.
If you can provide evidence that someone doesn't have enough money left to validate, you can take their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
Otherwise it would be possible to reduce the total number of validators without paying a fee.

Charge fee for delegation/channels of time * money * constant.

Add third type for consensus_flag in channel_block, for people who ran out of money and can't validate any more.