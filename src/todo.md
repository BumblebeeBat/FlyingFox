Add nonce checks in every tx type!!

every constants:max_reveal() we should make a backup of the consensus state that is under finality, and delete the old backup.
People who join the network download the most recent backup, and all the blocks since then. Blocks older than the most recent backup are ignored because of long-range attacks. This is a weak subjectivity blockchain.
accounts, all_secrets, d_accounts, blocks, block_pointers, channels, d_channels, entropy

blockchain api for nodes to share blocks and tx and messages.

messanger system that lets you pay to send messages to peers with commands.

hashlock payments using the messanger system.

garbage collect blocks. The whole file needs to be left shifted to delete the first part. block_pointers needs to be updated too. 

Add bets to channels, including the third signature.
There should be a bet that takes a list of weighted pubkeys and a list of signatures over the outcome, and performs Sztorc consensus on them.
There should be a language for combining the outcomes of bets, so that you can make custom bets.
There should be a bet where you use a merkle proof to prove a fact from the merkle root of the bet without revealing every fact about channel state.
There should be bets that check for the existence of a signature from a particular pubkey over some data.

we need to get rid of non_delegated as an option for channels.
It is bad because those channels don't cost anything to keep open, so someone could take up every channel.