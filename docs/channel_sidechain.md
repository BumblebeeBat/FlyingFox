Sometimes we want to link different channels together. The simplest example being a hashlock. If multiple channels use the same hashlock, then they can be simultaniously unlocked by revealing the secret.

A signature could work the same way. A branch of code in the contract could be locked down until a signature for a particular pubkey and over data that passes tests is available. If multiple channels were locked for the same pubkey/data combination, then it is similar to hashlock. The channels are connected.

The key that unlocks the new branch of code could be a 2/3 multisig.

The branch of code they unlock could recursively create the same contract, with a slightly updated list of pubkeys/balances. So this new branch of the contract is like a "next block". Since the channel nonce always gets higher as we tack on new branches of code, we can garbage collect all the parts of the contract that we know will never be used, the "old blocks".
So depending on consensus type, it could be 0 blocks of history, or a fixed amount, or all of them. Probably we will start with 2/3 consensus, because it is simple, and we already require everyone to come online multiple times per block.

The block can have a merkle root of a data structure, and have rules about how the root can be updated when we go to the next-block.
Probably all the pubkeys and balances should be stored in this datastructure. 
One rule could be that we can't change anyone's balance unless they sign an agreement to spend money (a "transaction"). In which case we change 2 simultanious, so the total number of coins is constant.

Eventually, we should have a rules about judging on the outcomes of prediction market predictions. Making a sidechain is the first goal.

Once we make the prediction market, we can use a seperate contract for each phase, so a lot of variables can be hard-coded, which should make it easier to program.
propose -> bet -> commit -> reveal