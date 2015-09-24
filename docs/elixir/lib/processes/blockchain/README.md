kv.ex holds too much state and needs to be split up into:
1) an account manager.
2) a blockchain that only contains pointers to the blocktree
3) the blocktree, which holds all the blocks.

