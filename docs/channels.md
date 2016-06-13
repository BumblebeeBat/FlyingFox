# Channels

A payment channel is a relationship between 2 people so that they can send money repeatedly, and only record their final settlement on the blockchain. 
This is more affordable than storing every money-transfer on the blockchain.
The rules of the payment channel are enforced by providing evidence to the blockchain about the current channel state.

It is possible to create secure channels with all these features:

1. You can spend money in them without publishing anything to the blockchain.
2. The money can be spent in either direction.
3. Each additional hash-locked transaction or bet increases the memory requirements linearly.
4. No expiration date requiring the channel be closed by any particular date.
5. You don't trust your partner.
6. No fee for spending funds through the channel.
7. Spend money as fast as sending a message, you don't have to wait for any confirmations.

Bitcoin channels will eventually be able to do everything except #9 and #3. For #3 in bitcoin, each additional hash-lock in a channel doubles the time and space requirements. `O(N^2)`

#### Limitations that channels have, compared with on-blockchain tx:

1. There is a finite amount of money in the channel. Taking money in or out of the channel requires posting to the blockchain.
2. Each channel has a number written on it called delay. If your partner disappears, it takes this long to get your money out. 
3. You need to log on once every delay, or else your partner could take the money. 

#### Example of the part of a channel that lives in the blockchain consensus state:

```
{
  pub1:Pubkey,
  pub2:Pubkey,
  amount1:Integer,
  amount2:Integer,
  delay:Integer
}
```

`amount1` is how much money `pub1` has deposited into the channel. `amount2` is how much money `pub2` has deposited into the channel.

#### Example of a channel contract that lives in channel state:

```
{
  nonce:Integer,
  pub1:Pubkey,
  pub2:Pubkey,
  bets:[Bet,Bet...],
  fast:Bool,
  amount:Integer
}
```

`amount` is how much money is moved from the `amount1` pile to the `amount2` pile on the blockchain. 

The entire channel state consists of 2 channel contracts. One is the most recent payment you sent your peer, and the other is the most recent payment your peer sent you.

`nonce` increases when new payments are made. Only the highest-nonced contract that was signed by both participants is valid.

#### Creating a channel, or increasing the amount of money in a channel:

```
{ 
  to:Bool,
  amount:Integer,
  delay:Integer,
  pub1:Pubkey,
  pub2:Pubkey
}
```
`to` tells which side of the channel to put your money into. It is possible to put money into channels you don't control. `delay` is only used when you first create a new channel. 

Preferably it should be possible to make one tx that both participants sign that moves some of each of their money into the channel. That way neither person has to go first.

#### Closing a channel normally:
Create a new channel block with `fast` set to `True`, have both parties sign the channel contract, and publish to the blockchain.

#### Closing a channel when your partner is gone:
Take the most recent channel contract that your partner paid you with, sign it, and publish it to the blockchain. You have to wait `delay` blocks, then you can publish a close-channel tx like

```
{
  pub1:Pubkey,
  pub2:Pubkey
}
```

#### Stopping your partner when they try to cheat:
Your partner has to wait at least delay amount of time before they can take the money, which give you time to publish the highest-nonced channel block.

#### About bets:
The total amount of money in a channel is conserved. Some money could be locked to either participant, and some money could be locked into bets.

Bets are written in a turing complete forth-like language.

Some of the better types of bets I have written so far: hashlock, oracle, and signature. 


All 3 types look like:

```
{
   amount:Integer,
   merkle:Merkle,
   default:Integer
}
```

`merkle` is the merkle root of a datastructure explaining the bet.


`default` is the part of money that goes to participant 2 if the bet is still locked when the channel closes. Extra money goes to participant 1.

#### Unlocking Bets:

##### Signature bets
example signature bet:

```
{
   amount:Integer,
   merkle:Merkle,
   to:Bool
}
```

are unlocked by 
```
{
   pubkey:Pubkey, 
   data:Binary, 
   sig:Signature          
}
``` 
where `sig` is a valid signature over `data` for `pubkey`.
`pubkey` and `data` needs to satisfy: `SHA256([pubkey, data])==merkle`.
If the channel closes and this bet is still locked, the money goes as default. If it is unlocked, then the money goes as amount.

##### Hashlock bets

example hashlock bet:

```
{
   amount:Integer,
   merkle:Merkle,
   to:Bool
}
```

are unlocked by a 256 bits called `secret`. It needs to satisfy `SHA256(secret)=Merkle`. If the channel is closed and this bet is still locked, the money goes opposite of `to`.

##### Burn bets
example burn bet:

```
{
   amount:Integer
}
```
burn bets cannot be unlocked. If the channel closes when money is in a burn bet, then that money gets burned.

##### Oracle bets
example oracle bet:
```
{
   amount:Integer,
   merkle:Merkle,
   default:Integer
}
```

are unlocked by 
```
{
oracle_pubkey:Pubkey,
bet_hash:Hash,
oracle_sig:Signature,
judgement:Integer
}
``` 
They need to satisfy:
```
SHA256([oracle_pubkey, bet_hash])=merkle
```
where `oracle_sig` is a valid signature for `oracle_pubkey`, over `SHA256([judgement, bet_hash])`. 

If it is unlocked, then judgement is the percentage of money that goes to participant 2, extra money goes to participant 1. If it is left locked, then `default` is how much goes to participant 2, and the extra goes to participant 1.

