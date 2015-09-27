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
- reveal
- to_channel
- channel_block
- close_channel

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
- Re-using existing predictions should be free.
- Should be possible to make some money in the channel state dependent on the existence of a signature valid for some given address and data. This way we can hash lock the existence of a signature to a payment.

#### close_channel:
Deletes the channel, and gives it's money to users

When bets are made, there is space for the oracle to write a number between 0 and 1, and sign. This determines how the outcome. This signature can be reused on other `channel_blocks` that were gambling on the same contract, so you don't have to consult the judge more than once for the same question. Eventually we want to allow the Sztorc consensus protocol for combining groups of judges' decisions.

It needs to be possible to hash-lock the judges' signing of the channel block with a `spend` on a different channel block. That way you can trustlessly pay the judge to provide his signature even if the judge isn't on a channel path between you and your partner.

Optionally allows you to simultaniously create a new channel between the same people with a smaller total amount of money in it.

This comes in 2 flavors: `slasher`, `normal`, and `timeout`.

If your channel partner uses a `channel_block` transaction to publish back in history, you should publish a `slasher` with a more recent `channel_block` that you and your partner both signed. The more recent history is the valid one.

If your partner won't show up, and you published a `channel_block`, eventually you can publish a `timeout` tx, and this closes the channel.

Normal requires both signatures. It closes the channel immediately, so you don't have to wait.



##Who gets paid, and when?

If you want to get an address to hold money, or you want to change the bounds for how much money you can have, then you have to publish a tx. This will take some time to be verified, and you may have to wait multiple confirmations. Doing this costs a fee, this fee pays the validators. The next block has a minimum cost, and can't be made until all these fees add up to enough money to afford the next block.

If you want to gamble or spend money within your bounds, you never have to publish a tx to the blockchain. Your participation will be instant. You still have to pay a fee, but it is far far lower. This fee does not go to the validators, it goes to the networking nodes that passed your message back and forth to your partner who you are gambling with.
