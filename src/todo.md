New transaction type, similar to slasher.
Each validator needs a minimum amount of money in their account.
If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
Otherwise it would be possible to reduce the total number of validators without paying a fee.

Add third type for consensus_flag in channel_block, for people who ran out of money and can't validate any more, or for people who want channels without the responsibility of delegating.

add another type of transaction for closing channels. This one should allow you to instantly close the channel, and take all the money, but only if your partner is very low on funds, and will soon lose his account.

reveal tx type

slasher tx type


every constants:max_reveal() we should make a backup of the consensus state that is under finality, and delete the old backup.
People who join the network download the most recent backup, and all the blocks since then. Blocks older than the most recent backup are ignored because of long-range attacks. This is a weak subjectivity blockchain.

accounts, d_accounts, blocks, block_pinters, channels, d_channels, entropy