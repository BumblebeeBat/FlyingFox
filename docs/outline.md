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
- channel_timeout
- channel_slash
- channel_close

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

When bets are made, there is space for the oracle to write a number between 0 and 1, and sign. This determines how the outcome. This signature can be reused on other `channel_blocks` that were gambling on the same contract, so you don't have to consult the judge more than once for the same question. Eventually we want to allow the Sztorc consensus protocol for combining groups of judges' decisions.

It needs to be possible to hash-lock the judges' signing of the channel block with a `spend` on a different channel block. That way you can trustlessly pay the judge to provide his signature even if the judge isn't on a channel path between you and your partner.

#### channel_timeout

If your partner is not helping you, this is how you start the process of closing the channel. 
You can only use the final channel-state, or else your partner can punish you for cheating.

#### channel_slash

If you partner tries closing the channel at the wrong point in history, this is how you provide evidence of the true final state, and punish him for cheating.

#### channel_close

If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

##Who gets paid, and when?

If you want to get an address to hold money, or you want to change the bounds for how much money you can have, then you have to publish a tx. This will take some time to be verified, and you may have to wait multiple confirmations. Doing this costs a fee, this fee pays the validators. The next block has a minimum cost, and can't be made until all these fees add up to enough money to afford the next block.

If you want to gamble or spend money within your bounds, you never have to publish a tx to the blockchain. Your participation will be instant. You still have to pay a fee, but it is far far lower. This fee does not go to the validators, it goes to the networking nodes that passed your message back and forth to your partner who you are gambling with.


Thoughts on bets:
Add bets to channels:
There should be a bet that takes a list of weighted pubkeys and a list of signatures over the outcome, and performs Sztorc consensus on them.
There should be a language for combining the outcomes of bets, so that you can make custom bets.
There should be a bet where you use a merkle proof to prove a fact from the merkle root of the bet without revealing every fact about channel state. 
There should be bets that check for the existence of a signature from a particular pubkey over some data.
There needs to be a bet so that if you are in a team for an SMPC, and you commit to storing a secret, the bet goes to 0 if your commitment does not match what you reveal, or if what you reveal doesn't match the rest of the team's secrets. The bet is 1 otherwise.
There needs to be a bet so that if you are in a team for SMPC, and you fail to participate, it deletes a little of everyone else's money, and a lot of yours.
There should be bets that output a value between 0 and 1 which is based on what the SMPC reveals. This is rather large: We need all the commitments. unlocking requires a lot of the secrets from the SMPC team.
We need a second version of the Sztorc consensus bet where the list of outcomes are secrets in the SMPC. So winning this bet requires having merkle proofs and signatures the from the SMPC to show the outcome of the bet. Each SMPC-participant signs once over the merkle root of their SMPC data, and then we use the root to prove the facts of what was in the SMPC.

A betting language would need to:
#1) opcode: to do Sztorc consensus. #we never have to do this on-chain.
2) opcode: to do sha256 on arbitrary sized data, to allow merkle proofs
3) opcode: check signature
4) opcodes: * / + - #convert integers to fraction when necessary.
6) opcodes: rot swap drop dup 2dup -rot tuck-n, pick-n
#8) opcode: to delete the money in the bet so neither party gets it. #unsolveable scriptPubs gives this functionality.
9) opcodes: if else then
10) opcodes: and or xor nand not
11) opcodes: > < == #convert integers to fraction when necessary.
12) opcode: stackdepth
13) opcodes: append_binaries, remove N bytes from right of binary, remove N bytes from left of binary.
14) opcode: flip stack
15) opcode: fail 
16) opcode: fraction2int #rounds down to the nearest integer.
16) opcode: int2fraction #takes 2 integers.
7) opcodes: aware of blockchain facts: totalcoins, height.

Code would mostly be integer codes like:
0, 1, 5, 30
But it would have Things mixed in:
<<>>, {f, 3, 2}, {integer, 33}

I could have the datastructure be a list of Things,
examples of Things:
* <<>>
* <<123, 44, 0, 1>>
* {f, 23, 100} % fraction.
* {f, 0, 1}
* {f, 230000000, 1}
* 27
* 0
* true/false

The code could be a list of Things and Opcodes. Things get pushed to the stack.

The bet is a list of bytes in the language. The key to unlock the bet and find it's outcome is another list of bytes.
Checking find the outcome of the bet is as easy as appending the bet to the key, and run the bytes through the VM.
When the script finishes, the Thing at the top of the stack is a fraction between 0 and 1 inclusive, and the second to top thing is a nonce. That fraction is the output, and is used to dermine how much of the money goes to each participant. The nonce is used for telling which way of solving the puzzle is correct. You can only solve the puzzle in the highest nonced way that you are able to, otherwise your partner could slash you.
If there is a stack underflow, or any other error processing opcodes, then it is an invalid tx.