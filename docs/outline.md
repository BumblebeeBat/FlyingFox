## Initial State

This outline has a lot of information in it that users of the blockchain wont ever need to be aware of. It is to help programmers understand how the code is organized, and some simple datastructures.

One address has all the money, and there is 1 genesis block. Additional blocks can be built on top of existing blocks to build a tree. The highest block in the tree is used to determine the current state. Each block contains transactions which modify the state from the block before.

###Rules for blocks to be considered valid:

1. All tx must be valid. 
2. Must reference hash of previous block.
3. The person who creates the block signs it.
4. Block creator pays a fee: `base_fee*2^(block_height-prev_block_height)`
5. Balances must stay positive.
6. Convince at least 37 of the possible validators to sign, there are 54 possible on average.
7. Amount of money bonded in each block can only change by a small percentage from the average of the last few blocks.
8. Amount of money bonded must be at least 2 times bigger than the amount spent in the block.

### Rules for tx:
Every address has a nonce that updates on each tx. To be valid, the tx must include the current nonce. Each tx must reference the hash of a recent block. Block-creator, tx-creator, and signers benefit if the tx references a more recent block.

### tx types:
- create_account
- spend
- delete_account
- sign
- slasher
- fork_slash
- reveal
- to_channel
- channel_block
- channel_timeout
- channel_slash
- channel_close
- channel_funds_limit
- repo
- make_oracle
- disband_oracle

#### spend:
For users to give money to each other. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.

#### sign:
Includes hash(entropy_bit+salt).
- ~54 signers are selected every block.
- A block requires at least 37 of them to sign for it to be valid.
- The balance of each signer is shrunk to pay a safety deposit.
- They all pay the same amount.
- The amount they pay is based off how much money is spent in the `spend` txs in this block.
- Total safety deposits needs to be 2.0x as big as the total amount of money spent in `spend` txs, that way if they double-spend they destroy as much money as they could be stealing.
- You can be selected as a signer based on how much money you have in channels.
- Only one of the 2 participants in a channel is taking part in the validation process.
- You can make a channel with yourself.
- If the money in a channel is selected as signer, you don't have to close the channel.
- You pay the safety deposit from your address.

If a channel is participating in the validation process, then one of the participants money will slowly shrink to zero. While their balance is non-zero, all the money in the channel is taking part in the validation process. This fee is used to pay people who actually make the `sign` transactions. It is a punishment for people who fail to participate.

#### slasher:
If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.

#### fork_slash:
The scenario we are trying to prevent looks something like this-
for all the even blocks validators sign on fork A, and for all the odd blocks they sign on fork B.
This transaction makes the sign transaction act like a commitment. Each validator is commiting to only sign on decendents of the 26th ancestor of the block they sign on. 26th cousin is ok, but 27th cousin is invalid.

#### reveal:
After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random `entropy_bit` and `salt` from the `sign` tx, and it reclaims the safety deposit given in the `sign` tx. (upgrade? If your bit is in the minority, then your prize is bigger.)

#### to_channel:
Spends money into a channel. Possibly creates new channel. This extends the amount of time the channel is involved with the consensus process. This can re-add the channel to the consensus process.

#### channel_block:
- Proposes a final state for this channel.
- No longer possible to `to_channel`. 
- Should be possible to have the channel state so some of the money is decided by the outcome of `contract + contract_state + oracle_id`.
- The merkelized hash of the contract and contract state and `oracle_id` are recorded in the channel state.
- Normally users give the correct money to the correct person before publishing the channel-block.
- If the channel-block still contains contracts when it is published, then we check to see if the oracle has signed.
- If the oracle did not sign, the money is deleted. If the oracle does sign, then the money is split up the way the oracle decided.

When bets are made, there is space for the oracle to write a number between 0 and 1, and sign. This determines how the outcome. This signature can be reused on other `channel_blocks` that were gambling on the same contract, so you don't have to consult the judge more than once for the same question. Eventually we want to allow the Sztorc consensus protocol for combining groups of judges' decisions.

It needs to be possible to hash-lock the judges' signing of the channel block with a `spend` on a different channel block. That way you can trustlessly pay the judge to provide his signature even if the judge isn't on a channel path between you and your partner.

#### channel_timeout

If your partner is not helping you, this is how you start the process of closing the channel. 
You can only use the final channel-state, or else your partner can punish you for cheating.

#### channel_slash

If you partner tries closing the channel at the wrong point in history, this is how you provide evidence of the true final state, and punish him for cheating.

#### channel_close

If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

#### channel_funds_limit

allows you to instantly close the channel, and take all the money, but only if your partner is very low on funds, and will soon lose his account.

#### repo

similar to slasher.
Each account needs a minimum amount of money.
If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.

#### pow

Can grow the market cap up to 0.1% per block. Price to produce new coins is determined by a mechanism the block creator participates in.
80 bytes total, {4:version, 32:hashPrevBlock, 32:MerkleRoot, 4:time, 4:difficulty, 4:nonce}
inside the work we need 1) address to be rewarded, 2) hash of a recent block. 
The more recent the block you reference, the bigger the reward.

### make_oracle

* transforms flying foxes into colored foxes
* signed by everyone who will participate
* one person is the manager. Everyone has a channel with him, your colored coins are in the channel.
* A number is given. This is the ratio of how much money the manager has in every channel. This money came from the manager's account.

### disband_oracle

* transform colored foxes back to flying foxes
* needs to be signed by the manager.
* closes all the channels.