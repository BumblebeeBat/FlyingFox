If you like Flying Fox, then maybe you want to start your own blockchain using the same source code.
When a user syncs their node to the network, they have to enter a recent hash from the blockchain.
If you launch your own fork of Flying Fox, then your blockchain will have different blocks from mine. If you give a recent hash from your blockchain to your friend, and they use that hash to sync their node, then their node will sync to your new chain.

Nodes that have list of conditions should never end up out of sync:
1) The absence of an adversary who is willing to buy up over 1/2 the coins and destroy it all.
2) Nodes need to power on at least once every constants:max_reveal() blocks (around 260 blocks currently).
3) In sync at one point in time.