We need a web wallet so that users don't have to install anything. The external API should get a lot bigger.
* We need javascript to handle the signing of transactions

For the web wallet to support channels, we need to make a javascript channel manager to keep track of your channel state.
* internal handler register
* internal handler channel_spend
* internal handler new_channel
* internal handler lightning_spend

Website advertising Flying Fox, with 3 big pictures and at least one gif, and a short catchphrase to describe the project.
It should collect email addresses of people who want to be in the oracle.
Eventually, this page will link to the crowdsale page.

A crowdsale app. it has tens of thousands of bitcoin addresses. People sign up with a flying fox address to be assigined a temporary bitcoin address. Then if they pay bitcoin to that address, then they will recieve some of the initial distribution of flying foxes.

Opcode to find out which account payed to create a block.

The random number generator needs to be updated.
It needs the property that only 1 bit of entropy is added per block, and the block creator shouldn't have control of this bit.
After the validators signed a block, we select one of them deterministicly randomly. When that person reveales, his randomnes mod 2 is the next random bit. If he fails to reveal, then a 0 is added instead.

When the block creator adds signatures to a block, he should have no idea which validator will have control of the random number.

To choose entropy chooser, xor all the secrets together to make the mega secret. xor the mega secret with each secret to get the rank. The highest rank mod 2 is the next bit of entropy.

10th Random number = RN_10
append bytes operation = <>
RN_N = hash256(RN_N-1 <> next_bit)

Currently the plan make it hard to DDOS validators is by an onion routing network.
Another choice is to have each validator pre-compute a chain of data such that each link hashes to the next link of the chain.
Every time they validate they reveal the next link of their chain.
If anyone knows a validator's next link, they should be able to punish the validator.

Building an executable of the software for windows is very important. It needs to include logging, for easy error reporting. That way if a user experiences an error, it is easy for them to show us what went wrong.

http://stackoverflow.com/questions/11796941/how-do-you-compile-an-erlang-program-into-a-standalone-windows-executable


channel_block_tx:fee needs to be used. If someone closes the channel early, the person who didn't close the channel pays this fee to the person who did. Unless there isn't enough money left, in which case the person who closes the channel gets all the non-deleted money.

We need to add a fee to sign_tx. This fee can possibly be negative.

Consider updating sign_tx to the bet based system used in Casper. Validators can sign on conflicting branches, as long as their total bets add to something <= 1. 

It is inefficient to store things in base64 mode. keep it decoded as much as possible.


It would be nice if we have a javascript interface to send spam-less messages. The recipient has the option of deleting the sender's funds.

close_channel javascript multiple steps
*Look in channel_partner. Make sure our partner has = or < nonce than channel_manager.
change server javascript
delete_account javascript
button to rotate through your inbox

Make an OTP erts package so that it is easier to install.

handler should have every input and output be encrypted. Otherwise eavesdroppers will publish our channel before we want it published.

We need tests to make sure that skipping a height works. It shouldcost more for the block creator.

constants:security_bonds_per_winner() should be tuned. The random number generator should be seeded from a long enough time ago.
We want it to be impossible to cause a fork by bribing validators to double-sign. There shouldn't be enough money in the blockchain to maintain the fork long enough to have different randomness on each side.
Block tree needs to hold many more blocks. Lets try to keep it below 200 megabytes of ram or so. We need to know everyone's balance long enough ago because the random seed is from a long time ago. We want it to cost >50% of the money in the blockchain to cause a fork. (maybe we don't need to save the whole block. We only need to remember the random entropy and everyone's balances.)


the idea was introduced here: https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/
Weak subjectivity is necessary for security reasons. It stops long range attacks.
POW solves the same problem too, so long as people are still mining.

Add onion routing to messaging

https://blog.ethereum.org/2016/02/17/smart-contracts-courts-not-smart-judges/
Maybe upgrade the off-chain code so that it is computed by binary search.
