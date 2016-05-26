Flying Fox: A Peer-to-Peer Electronic Betting System

Zack Hess

Abstract. A purely peer-to-peer version of electronic betting would allow pairs of people to make bets directly one party to another without going through a financial institution. 

1. Introduction

Gambling and fundraising are heavily regulated world wide. The regulatory costs are high. Legal crowdfunding companies like kickstarter are heavily limited in the types of funding contracts they can offer. Many important financial services are illegal, onion futures in particular. All these fees and rules make it too complex and expensive for most people to participate.

What is needed is an electronic gambling system based on cryptoeconomic proof instead of trust, allowing any two willing parties to bet directly with each other without the need for a trusted third party. 

In this paper, we propose a platform for trustlessly enforcing programmable gambling contracts. The system is secure as long as honest nodes collectively control more than 1/2 of the coins.

2. State Channels

A state channel is a relationship between 2 accounts which allows them to transfer money back and forth without recording anything to the blockchain. They are also used to make and resolve bets.

3. Betting through a 3rd party

Having a channel costs money, so you wouldn't want to have too many. If you have a channel with a popular node, you can bet through them to anyone else. The downside of doing this is that the money has to be locked up in both channels at once. The person in the middle is collecting arbitrage.

4. Moving a bet to a more direct path

If your bet is on an expensive multi-step path, you can move it to a cheap one-step path on the next block. You create a new channel directly to the person you are betting with. You use a hashlock to simultaniously cancel the bet on one path and activate it on the other.

5. Bonded Proof of Stake

Bonded Proof of Stake is a type of consensus where some of the users put their money into bonds, making the money innaccessible for a time. They participate in choosing the next block for the chain. If they break any rule, their bond is taken away. If they follow all the rules, they get paid transaction fees from the users.

6. Delegated Proof of Stake

is a type of consensus where the users elect someone to be their representative. The representatives choose what block to add to the chain next.

7. Lightning Consensus

Lightning Consensus is a Bonded Proof of Stake system that also has Delegated Proof of Stake properties. Each channel chooses one of the two participants to be the representative. Your likelyhood of being selected as validator increases the more money you have in channels. The economic situation of choosing someone to be your channel participant is such that it is very expensive to buy votes. If you sell your votes to someone, the money in the channel is innaccessible as long as you keep choosing them as your representative. 

A random number generator is used to elect about 50 validators per block.
Each validator has to provide a security deposit when they sign the fork they choose.
If they sign 2 different forks at the same height then the validator loses the safety deposit.

8. Reclaiming Disk Space

Flying Fox consensus only needs to be aware of the 200 or so most recent blocks. Older blocks are deleted. This keeps the memory requirements to a small finite size.

References

[0] V. Buterin "Slasher: A Punitive Proof-of-Stake Algorithm" https://blog.ethereum.org/2014/01/15/slasher-a-punitive-proof-of-stake-algorithm/

[1] S. Nakamoto "Bitcoin: A Peer-to-Peer Electronic Cash System" https://bitcoin.org/bitcoin.pdf 2009.

[2] P. Sztorc http://bitcoinhivemind.com/