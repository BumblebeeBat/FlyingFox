Channels

It is possible to create channels with all these properties:

1) You can spend money in them without publishing anything to the blockchain.

2) The money can spend in either direction.
3) Each additional hash-locked transaction or bet increases the memory requirements linearly.

4) No expiration date on the channel.

5) You don't trust your partner.

Bitcoin channels will eventually be able to do everything except #3.

Limitations that channels have compared with on-blockchain tx:

1) There is a limit of how much money is in the channel, you have to post on-chain to change this limit.

2) Each channel has a delay. If your partner disappears, it takes this long to get your money out. You need to log on once every delay, or else your partner could take the money. 

Example of the part of a channel that lives in the blockchain consensus state:

{pub1:Pubkey,

pub2:Pubkey,

amount1:Integer,

amount2:Integer,

delay:Integer}

amount1 is how much money pub1 has deposited into the channel. amount2 is how much money pub2 has deposited into the channel.

Example of a channel contract that lives in channel state:

{nonce:Integer,

pub1:Pubkey,

pub2:Pubkey,

bets:[Bet,Bet...],

fast:Bool,

amount:Integer}

amount is how much money is moved from the amount1 pile to the amount2 pile on the blockchain. 

The entire channel state consists of 2 channel contracts. One is the most recent payment you sent your peer, and the other is the most recent payment your peer sent you.

The nonce increases when new payments are made. Only the highest-nonced signed contract is valid.

Creating a channel, or increasing the amount of money in a channel:

{to:Bool,

amount:Integer,

delay:Integer,

pub1:Pubkey,

pub2:Pubkey}

to- tells which side of the channel to put your money into. It is possible to put money into channels you don't control.
delay- this is only used when you first create a new channel. 

Closing a channel normally:
Change "fast" to True, have both parties sign the channel contract, and publish to the blockchain.

Closing a channel when your partner is gone:
Take the most recent channel contract that your partner paid you with, sign it, and publish it to the blockchain. You have to wait "delay" blocks, then you can publish a close-channel tx like

{pub1:Pubkey,
pub2:Pubkey}

Stopping your partner when they try to cheat:
Your partner has to wait at least delay amount of time before they can take the money, which give you time to publish the highest-nonce channel block.

about bets:
The total amount of money in a channel is conserved. Some money could be locked to either participant, and some money could be locked into bets.

There are at least 3 types of bets: hashlock, oracle, and signature. All 3 types look like: {amount:Integer, hash:Hash, default:Integer}.
Hash is the merkle root of a datastructure explaining the bet.

Each type of bet gets unlocked in a different way. If the channel block gets published and a bet is not unlocked, then where the money goes is determined by default.

If a bet is a hashlock bet, then it should be possible to find 256 bits called the secret such that SHA256(secret)=Hash. The secret unlocks this type of bet.

If the bet is an oracle bet, then SHA256([oracle_pubkey, bet_hash])=Hash, where bet_hash is the hash of the ascii text explaining the bet. To unlock this type of bet you need {oracle_sig:Signature, judgement:Integer} where oracle_sig is a valid signature for the oracle_pubkey, over SHA256([judgement, bet_hash]). If it is unlocked, then judgement is used to dermine how much of the money moves.

If it is a signature bet, then SHA256([pubkey, bits])=Hash.
To unlock this type of bet you need {sig:Signature} where sig is a valid signature for pubkey over bits.
