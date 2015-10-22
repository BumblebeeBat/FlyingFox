add another type of transaction for closing channels. This one should allow you to instantly close the channel, and take all the money, but only if your partner is very low on funds, and will soon lose his account.
channel_funds_limit_tx



every constants:max_reveal() we should make a backup of the consensus state that is under finality, and delete the old backup.
People who join the network download the most recent backup, and all the blocks since then. Blocks older than the most recent backup are ignored because of long-range attacks. This is a weak subjectivity blockchain.
accounts, d_accounts, blocks, block_pinters, channels, d_channels, entropy

blockchain api for nodes to share blocks and tx and messages.

messanger system that lets you pay to send messages to peers with commands.

hashlock payments using the messanger system.

garbage collect blocks. The whole file needs to be left shifted to delete the first part. block_pointers needs to be updated too. 