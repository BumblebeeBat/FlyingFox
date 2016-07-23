The thing below with leaders does not work. If the leader and reporters collude, they can steal all the money being gambled.
v
Using reputation as money is better because after an attack the reputation's value can go to zero.




How do we put truthcoin into the channels mechanism?

(I mention a "leader" several times. To make this process decentralized, the leader can be replaced by a N of M group of managers)

Write these channel contracts:

1) We need a contract so that oracle participants can commit to judging exactly once. There needs to be enough judging safety deposits to be 3x bigger than the amount of money gambled on the bigger side. They make this commitment to the leader. (3 times bigger because only 1/3rd of reporters have to double-report for 2 outcomes to seem valid. You need 2/3rds for an outcome to seem valid)

2) we need a contract for oracle reporters to commit to their judgment, so that they have to reveal it later, and can't change their mind. Reporters make this commitment to the leader.

3) We need a contract so that if a majority of oracle participants commit in the same direction, those commitments can be used to unlock a payment. The gamblers will use this contract with the leader.

4) We need a leader to commit to deciding who the honest oracle participants are. He needs to commit to making this decision exactly once. When he does decide, this distributes a large amount of money as fees to the honest oracle reporters.
This confiscates the safety deposit from the dishonest reporters. The confiscated money is split between the gamblers, and the honest reporters.
We need to pay gamblers, because otherwise the reporters and leader could collude to rob the gamblers.
We need to pay the honest reporters to incentivize any colluding attackers to have a sub-group of counter-attackers.
This contract is between the leader and the reporters and the gamblers.

gambler contracts:

1) calculate how much money I earn from dishonest reporters.
2) add on any gambling winnings I earned.
3) pay me my money.

reporter before revealing judgement:

1) I commit to revealing this secret when asked. If I fail to reveal, then pay the gamblers my safety deposit. Do not give it to validators, or to the leader. This is to make sure there is no incentive to censor someone from revealing.






gamblers need to verify that the leader and his reporters deleted money to punish validators who double-report. If double-reporting happens, the validators money should be distributed to the gamblers.



What it would look like if we did it on-chain, with tx-types:

1) propose new market. this starts a dominant assurance contract to raise money for a market. The proposer is the entrepenuer. The market is considered a success if enough money is raised by the timelimit, and a failure otherwise. If it succeeds, it creates a market for gambling.

2) invest money in a dominant assurance contract.

3) collect earnings from a failed dominant assurance contract.

4) bet in a market

5) agree to be an oracle for a market. This is a large tx signed by everyone who will participate as an oracle. This creates a new currency out of the money.

6) commit to a report. only oracle participants make this tx.

7) reveal the report you committed to.

8) convert the reputation back to fungible currency. (Validators censor this if they dislike something the oracle did)

Half-way

1) propose new market. this starts a dominant assurance contract to raise money for a market. The proposer is the entrepenuer. The market is considered a success if enough money is raised by the timelimit, and a failure otherwise. If it succeeds, it creates a market for gambling.

2) invest money in a dominant assurance contract.

3) collect earnings from a failed dominant assurance contract.

4) bet in a market
