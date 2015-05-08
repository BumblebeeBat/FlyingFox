Initial state is for one address to have all the money, and for there to be 1 genesis block. Additional blocks can be built on top of existing blocks to build a tree. The highest block in the tree is used to determine the current state. Each block contains transactions which modify the state from the block before.

Rules for blocks to be considered valid:
all tx must be valid. 
must reference hash of previous block.
The person who creates the block signs it.
Block creator pays a fee: base_fee*2^(block_height-prev_block_height)
balances must stay positive.
Convince at least 37 of the possible validators to sign, there are 54 possible on average.

Rules for tx:
Every address has a nonce that updates on each tx. To be valid, the tx must include the current nonce. 

tx types:
spend
spend2wait
wait2bond
bond2spend
sign
slasher
reveal
to_channel
channel_block
close_channel

spend:
For users to give money to each other. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.

spend2wait:
convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.

wait2bond:
If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you don't participate, then the value will quickly shrink. 
There is a minimum size for purchasing bond-money, priced in money. 

bond2spend:
Users can take their money out of the bond at any time. 

sign:
Includes hash(entropy_bit+salt).
~54 bond-holders are selected every block. A block requires at least 37 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.

slasher:
If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.

reveal:
After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. (upgrade? If your bit is in the minority, then your prize is bigger.)

to_channel:
Spends money into a channel. Possibly creates new channel.
take money from sender's address, and puts it into the channel.

channel_block:
proposes a final state for this channel. no longer possible to to_channel. Needs to include entire channel state.

close_channel:
deletes the channel, and gives it's money to users


##update types for phase 2:

channel_block:
Should be possible to update the channel state so some of the money is decided by the outcome of a contract + contract_state + oracle_id. The merkelized hash of the contract and contract state and oracle_id are recorded in the channel state.
Normally users give the correct money to the correct person before publishing the channel-block. If the channel-block still contains contracts when it is published, then the oracles are in charge of that money.
In the event of disagreement, the correct oracle judges over the outcome of the decision.

Allow creation of oracles. 

Allow oracles to make transaction which determine where funds being gambled upon in a channel state go. Users need to send the channel-code, and channel-state to the correct oracle who can then send the money to the correct person.