need to use the arbitrage gen_server, in channels.
Use it to keep track of which pairs of channels are linked.
If 2 channels have linked state, it is important that we remove the links in the right order, or else we could get robbed.

Need to add some transaction types for the new type of mining.
80 bytes total, {4:version, 32:hashPrevBlock, 32:MerkleRoot, 4:time, 4:difficulty, 4:nonce}
Maybe I should start out owning $1,000,000 of coins or so, otherwise it will be too cheap for a whale to print a bunch of coins and refuse to take part in consensus. The $1,000,000 of coins could be unspendable, only for consensus purposes.

We should change the way flying fox scripts get merkelized. Maybe it should be like an case-opcode, where the code that gets executed needs to match a hash.
Maybe we should be able to call a "function" by the hash of the list of opcodes. add define to the language. We only have to define the functions that get used. If we add "define" then we also need to add gas, and it would be turing complete.


https://blog.ethereum.org/2016/02/17/smart-contracts-courts-not-smart-judges/
Maybe upgrade the off-chain code so that it is computed by binary search.

It would be nice if we have a javascript interface to send spam-less messages. The recipient has the option of deleting the sender's funds.

hashlock lightning payments javascript
close_channel javascript multiple steps
change server javascript
delete_account javascript
button to rotate through your inbox

Make an OTP erts package so that it is easier to install.

handler should have every input and output be encrypted. Otherwise eavesdroppers will publish our channel before we want it published.

We need tests to make sure that skipping a height works. It should cost more for the block creator.

constants:security_bonds_per_winner() should be tuned. The random number generator should be seeded from a long enough time ago.
We want it to be impossible to cause a fork by bribing validators to double-sign. There shouldn't be enough money in the blockchain to maintain the fork long enough to have different randomness on each side.
Block tree needs to hold many more blocks. Lets try to keep it below 200 megabytes of ram or so. We need to know everyone's balance long enough ago because the random seed is from a long time ago. We want it to cost >50% of the money in the blockchain to cause a fork. (maybe we don't need to save the whole block. We only need to remember the random entropy and everyone's balances.)


the idea was introduced here: https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/
Weak subjectivity is necessary for security reasons. It stops long range attacks.

Add onion routing to messaging

Maybe instead of sending channel messages to their mail box, we should give them an api where they can look up if the channel was updated?

