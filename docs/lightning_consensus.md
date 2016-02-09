Lightning Consensus
===================

A cryptocurrency is money built out of software that is difficult to counterfeit. 

The consensus protocol is the rules that participants must obey to participate in a cryptocurrency. The rules are carefully chosen so that it is expensive to control money that you don't own. The most popular cryptocurrency consensus protocols today are of the proof-of-work variety. Other proposals are: delegated proof-of-stake, various security bond proof of stakes, byzantine fault tolerant algorithms, and coin-age proof of stake.
Lightning Consensus is a new consensus protocol proposed in this paper. It is designed to be less expensive to operate than the existing methods, and more scalable. 

A payment channel is a relationship between 2 people so that they can send money repeatedly, and only record their final settlement on the blockchain, which is more affordable than storing every money-transfer on the blockchain. The rules of the payment channel can be enforced by providing evidence to the blockchain about the current channel state. The lightning network is created by connecting multiple channel-payments together so that either all of them are valid, or none of them are. So if Alice has a channel with Bob and Bob has a channel with Charlie, Bob can facilitate a money transfer from Alice to Charlie without any of them having to trust each other.

A problem with proof of stake systems that are based on how much money you have is that most people are off-line most of the time. If a large portion of money is off-line, the money that is on-line has a larger portion of control. Making people leave their computer on 24/7 with an unencrypted private key in the ram is annoying and dangrously insecure. We need a way to delegate control to a subset of participants who are willing to leave their computers on.

Some problems with full-on delegated proof of stake is that delegates will invest in advertising, which makes this consensus method expensive, and delegates don't have to be invested in the currency to get powerful.

There is a problem with the lightning network. Putting your money into a channel reduces the number of people that the money can be spent to, which reduces network liquidity and destroys value. This is similar to the problem that security-bond proof-of-stake consensus protocols have. If you lock up money, that is the same as destroying the interest rate of that money. 

-------> The value that gets destroyed for maintaining channels can simultaneously be used to secure blockchain consensus, so we don't have to destroy value twice. We can kill 2 birds with 1 stone. <--------

One of the 2 participants in the channel is delegated as validator. He has a small probability to participate in the consensus process for each block. The probability he can participate is based on how much money is in the channel. The validator needs to be online to sign blocks.

The most powerful validators will have servers running 24/7 in multiple locations, that way they can have fast latency to keep customers. They will pay advertising to attract customers. They will have ongoing profits that they would lose if the blockchain broke. They will be providing a real service other than heaters: processing channel payments.

Flying Fox is the first blockchain to implement lightning consensus.

related article: http://www.truthcoin.info/blog/pow-cheapest/


The critical value we care about for blockchain consensus is the (cost to control 0.01% more of consensus for people who do not own any coins)/(cost to control 0.01% more of consensus for people who own 10% of coins.) I call it the efficiency of the blockchain. For POW it is 1, because you mine at the same rate regardless of how many coins you own. For every POS proposed, besides Flying Fox, it is about 1, the cost of locking up coins is the same, whether you own the coins or not. The cost of buying votes is the same, whether you own the coins or not. 
In Flying Fox, the effeciency is >100.

If you don't own any money, but want to control consensus, this is possible in Flying Fox. You open a channel with someone who does have money, have yourself be the validator, and pay them to keep some money in this channel.
Since you have no money, you can't facilitate channel payments to anyone else. Your partners money is locked up and innaccessible. Your partner is losing value by the interest rate. You need to pay them at least this much.

If you do own 10% of money, then you can make tons of channels with lots of people. If you want to control a little more consensus, it is really easy to convince a partner to store money in a channel with you. You can facilitate money transfers anywhere they wanted to spend anyway. Their money is totally accessible. You might not need to pay them at all.