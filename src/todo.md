change unsafe_write so that the block doesn't change any state. It is just loaded into the blocktree.
Start unsafe write on one block earlier.
Use normal write on the block that had been unsafe.


download_blocks line 79.
I need to start downloading blocks from finality before the root, to fill up ram with blocktree.

I need to add that block to the blocktree as a root to build off of.
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

We need tests to make sure that skipping a height works. 
