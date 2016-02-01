Putting value onto a blockchain usually involves destroying an equivalent amount of value somewhere else. This document is NOT about the cost of creating new coins.
Once we have value on the blockchain, maintaining consensus about everyone's balances involves destroying some value. If it was free to add blocks to the chain, then we would run out of space very quickly. If it was free to propose valid forks, then the consensus would be very forked, and would not function. This document is about techniques to minimize the value that gets destroyed to maintain consensus.

The consensus used in Flying Fox is the cheapest way to secure consensus. You have to understand channels to know why. Channels are necessary for scalability, they move transactions off-chain. Putting your money into a channel reduces network liquidity, which destroys value. The value that gets destroyed for maintaining channels can simultaneously be used to secure blockchain consensus. The total value destroyed is orders of magnitude less than any other consensus method.

==Why proof of stake is similarly expensive to pow==
Paul Sztorc explains it very well. http://www.truthcoin.info/blog/pow-cheapest/
Taking part in the consensus process for proof of stake means putting up a bond. The bond-holders work together to make the next block. A person with 10% of bonds has 10% control over what the next block will look like.

This is expensive because the money that is in bonds cannot be used for anything else. Value is destroyed based on the interest rate.

If an attacker manages to buy up large amounts of coins, he could bond them all and destroy the cryptocurrency. So for POS to work we need to have a very large portion of the money bonded. ideally > 50%. 
Bonding money so it is uspendable is identical to destroying the interest rate of that money. POS is excessively expensive.

==Why delegated proof of stake doesn't work==
When you vote for someone based on how much money you have, your money is still spendable. So money isn't destroyed base on the interest rate like in POS. look at presidential elections to see how much money can be wasted on advertising candidates. The candidates don't have to have any investment in the blockchain to incentives themselves to be honest.  Candidates buy votes. Attackers can pay candidates to attack.

==About channels==
Channels transactions have some limitation in comparison to normal transactions.
1) There is a finite amount of money in each channel. spending money in the channel changes what percentage of the money each of the 2 participants controls. 
2) If your partner disappears, you have to wait a delay until you can get the money out.
3) If your partner closes the channel at the wrong point in history, then you have delay amount of time to provide counter-evidence and stop your money getting stolen. 

Usually, one of the channel participants is very rich and very well connected. He is hired for his ability to cheaply move your money to many places. He is also hired to give you extra liquidity in your channel so that you can receive payments. This person has their node running 24/7 to process payments.

The other channel participants is not well connected. He only logs on occasionally to spend money.

==The cheapest consensus mechanism==
If we combine the channel relationship with the delegation vote, then we can be sure that the validator is well invested in the system.
The only way to receive a lot of votes is by providing lots of liquidity.

So for this POS, instead of making bonds, you make a channel. For each channel, one of the pair of participants is the validator. If the channel has twice as much money, then the validator has twice as much control over adding blocks to the blockchain.

This doesn't have the same problem as bonded POS. Your money is still spendable. You aren't destroying value based on the interest rate.

There will be a market rate for the balance between liquidity and validating power. For example, say the rate is 1/6
If I as a customer wanted to put 1000 coins into a channel with a liquidity provider, then the liquidity provider would be willing to put 5000 coins on the other side of the channel, and neither of us pays the other.
If I needed the ability to receive more than 5000 coins, then I would have to pay the liquidity provider to provide a bigger limit.
If I didn't need to receive money, and was only planning on spending, then I would set the limit to 0, and they would pay me for giving them more validating power.
