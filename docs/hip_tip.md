hivemind improvement proposal / truthcoin improvement proposal. 

Abstract

the purpose of this document is to explain a way to reduce the operating costs of of blockchain prediction markets.

Motivation

Recent research in blockchain consensus technology has come to a general rules regarding safety deposts:
* Don't reuse safety deposits at different times. 
If a safety deposit is used to disincentivize multiple actions at different times, then the safety deposit will need to be locked up for a longer time period that contains all the actions we want to disinventivize. Leaving the safety deposit locked for a longer time period costs more money.
* Don't reuse safety deposits for different things at the same time. 
Once a safety deposit is destroyed to punish one action, there is no deposit left to disincentive other actions. 
So a small failure can propagate into a bigger one.
* Don't collect a safety deposit until you know how big it will need to be.
Otherwise we could end up locking up too much money at once, which would make the protocol more expensive. Or we could lock up not-enough, which means we have to unlock the security deposit, and then collect a bigger one.

Problem

The cost of operating a prediction market is the same as (the total amount of money locked in security deposits) * (the amount of time they are locked up for) * (the interest rate).
The minimum amount of security deposits we need at any one time is the same as 3x the volume of bets being judged over in that moment.
The old design used the sub-currency called "reputation" or "votecoins" as the security deposit. 
The reputation/votecoins is usually bigger than the necessary minimum, so it is more expensive than the most optimal method.

Definitions

Validators for a blockchain are the nodes that participate in the blockchain consensus mechanism. They work together to add blocks to the blockchain. 

Solution

Reputation/votecoins is a mistake. Instead, we shouldn't collect safety deposits until after betting in a market terminates, and we need a judgement to determine the outcome. We wait until this last moment to collect safety deposits, because this is the first moment when we know how much safety deposts we will need.

The safety deposits for a single market should be made up of a single kind of colored coin. The validators for the blockchain should be able to censor the tx that would de-colorize these coins. That way, if the oracle tries to cheat, the validators have a simple way to destroy the safety deposit.

Code

Example

if there are 1000 markets, each with $10,000 of gambling, then there would be $10 million being gambled in truthcoin.

Using the old reputation as a currency method, there would need to be a minimum of $10 million locked up in reputation 24/7, very expensive.

Using my new method only $30,000 would need be locked up in safety deposits at any one moment. It only takes 10 minutes to collect votes. Locking up $30,000 for only 10 minutes is very affordable.

Notes

[We can do all this off-chain](colored_lightning.md)

Attribution


