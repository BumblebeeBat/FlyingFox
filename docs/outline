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
After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.



Transaction types for phase 2:

New channel:
This creates a 2-way channel between 2 people. It allows them to give money to each other without writing anything onto the blockchain. It must be signed by both of them.
It moves money from either or both accounts into the channel.

Channel:
This is how you spend money without writing on the blockchain. both need to sign. It has a nonce.
You can update to higher nonces, but you can never publish a tx with a lower nonce.
The tx includes the entire current state of the channel.
The fee can come from either or both.
