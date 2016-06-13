Lightning Consensus
===================

A cryptocurrency is money built out of software. 

The consensus protocol is the rules that participants must obey to participate in a cryptocurrency.
The rules are carefully chosen so that it is expensive to control money that you don't own.
The most popular cryptocurrency consensus protocols today are of the proof-of-work variety.
Other proposals are: delegated proof-of-stake, various security bond proof of stakes, byzantine fault tolerant algorithms, and coin-age proof of stake.
Lightning Consensus is a new consensus protocol proposed in this paper.
It is more scalable and less expensive to operate than the existing methods, such as:

* [DPOS](docs/delegated_pow_problem.md)


There is a problem with the lightning network. Putting your money into a channel reduces the number of people that the money can be spent to, which reduces network liquidity and destroys value. This is similar to the problem that security-bond proof-of-stake consensus protocols have. If you lockn up money, that is the same as destroying the interest rate of that money. 

The value that gets destroyed for maintaining channels can simultaneously be used to secure blockchain consensus, so we don't have to destroy value twice. We can kill 2 birds with 1 stone.

One of the 2 participants in the channel is delegated as validator.
He has a small probability to participate in the consensus process for each block.
The probability he can participate is based on how much money is in the channel.
The validator needs to be online to sign blocks. Every time the validator signs a block, he locks some money up as a security deposit.
If he double-signs onto a different chain, this security deposit is taken away.
You cannot use the same money for a security deposit and a channel at the same time.

The most powerful validators will have servers running 24/7 in multiple locations, that way they can have fast latency to keep customers. They will pay advertising to attract customers. They will have ongoing profits that they would lose if the blockchain broke. They will be providing a real service other than heaters: processing channel payments.

Flying Fox is the first blockchain to implement lightning consensus.

related article: http://www.truthcoin.info/blog/pow-cheapest/

If you don't own any money, but want to control consensus, this is possible in Flying Fox. You open a channel with someone who does have money, have yourself be the validator, and pay them to keep some money in this channel.
Since you have no money, you can't facilitate channel payments to anyone else. Your partners money is locked up and innaccessible. Your partner is losing value by the interest rate. You need to pay them at least this much.

If you do own 10% of money, then you can make tons of channels with lots of people. If you want to control a little more consensus, it is really easy to convince a partner to store money in a channel with you. You can facilitate money transfers anywhere they wanted to spend anyway. Their money is totally accessible. You might not need to pay them at all.