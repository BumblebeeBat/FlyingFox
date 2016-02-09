line 89 of download blocks has an idea.

When we try to sync, constants:accounts(), and constants:all_secrets() are not identical to the backup version. 

http://zackbitcoin.pythonanywhere.com/looking at backup.erl
working to allow syncing from further behind than finality.
look at block_tree around line 74. Probably we need to do a block_finality:append(block, height) to stick our new block into finality. Maybe we don't need to put it into the blocktree? or maybe we need both.

hashlock lightning payments javascript
close_channel javascript multiple steps
change server javascript
delete_account javascript
button to rotate through your inbox

Make an OTP erts package so that it is easier to install.

To make hashlocked tx with scripts more affordable, we should let bet reveals reference blocks that were revealed since max_reveal. That way you don't have to reveal the same data onto the chain twice for a 2 step hashlocked lightning.

are we using arbitrage yet?

update download_blocks so that we can reach consensus with a node that is further in the future than finality.

handler should have every input and output be encrypted. Otherwise eavesdroppers will publish our channel before we want it published.

update now()

We need tests to make sure that skipping a height works. It should cost more for the block creator.

constants:security_bonds_per_winner() should be tuned. The random number generator should be seeded from a long enough time ago.
We want it to be impossible to cause a fork by bribing validators to double-sign. There shouldn't be enough money in the blockchain to maintain the fork long enough to have different randomness on each side.

Block tree needs to hold many more blocks. Lets try to keep it below 200 megabytes of ram or so. We need to know everyone's balance long enough ago because the random seed is from a long time ago. We want it to cost >50% of the money in the blockchain to cause a fork. (maybe we don't need to save the whole block. We only need to remember the random entropy and everyone's balances.)