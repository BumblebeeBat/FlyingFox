need to use the arbitrage gen_server, in channels.
Use it to keep track of which pairs of channels are linked.
If 2 channels have linked state, it is important that we remove the links in the right order, or else we could get robbed.

need to update channel manager to save 2 copies of the channel state. One is the highest nonced that I have signed, the other is the highest nonced that my partner has signed that I know about.
It is important to know about your partner's highest nonce, because we have to use arbitrage to unlink in the correct order. If you don't know your partner's current state, then you can't know if they are still linked.



We should change the way flying fox scripts get merkelized. It should be like an case-opcode, where the code that gets executed needs to match a hash.

https://blog.ethereum.org/2016/02/17/smart-contracts-courts-not-smart-judges/
Upgrade the off-chain code so that it is computed by binary search.

javascript to stop the node: `flying_fox_sup:stop()`

It would be nice if we have a javascript interface to send spam-less messages. The recipient has the option of deleting the sender's funds.

automatically ask for recent hash from 52.36.106.100:3010, then start downloading from anyone who agrees with that hash.
download_blocks:sync({52,36,106,100}, 3010).

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