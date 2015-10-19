New transaction type, similar to slasher.
Each validator needs a minimum amount of money in their account.
If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.
Otherwise it would be possible to reduce the total number of validators without paying a fee.

add another type of transaction for closing channels. This one should allow you to instantly close the channel, and take all the money, but only if your partner is very low on funds, and will soon lose his account.



every constants:max_reveal() we should make a backup of the consensus state that is under finality, and delete the old backup.
People who join the network download the most recent backup, and all the blocks since then. Blocks older than the most recent backup are ignored because of long-range attacks. This is a weak subjectivity blockchain.
accounts, d_accounts, blocks, block_pinters, channels, d_channels, entropy

blockchain api for nodes to share blocks and tx and messages.

messanger system that lets you pay to send messages to peers with commands.

hashlock payments using the messanger system.


