Add nonce checks in every tx type!!

blockchain api for nodes to share blocks and tx and messages.

messanger system that lets you pay to send messages to peers with commands.

hashlock payments using the messanger system.

garbage collect blocks. The whole file needs to be left shifted to delete the first part. block_pointers needs to be updated too. 

Add bets to channels, including the third signature.
There should be a bet that takes a list of weighted pubkeys and a list of signatures over the outcome, and performs Sztorc consensus on them.
There should be a language for combining the outcomes of bets, so that you can make custom bets.
There should be a bet where you use a merkle proof to prove a fact from the merkle root of the bet without revealing every fact about channel state.
There should be bets that check for the existence of a signature from a particular pubkey over some data.

