How do we put truthcoin into the channels mechanism?

Write these channel contracts:

1) We need a contract so that oracle participants can commit to judging exactly once. There needs to be enough judging safety deposits to be 3x bigger than the amount of money gambled on the bigger side. They make this commitment to the leader. (3 times bigger because only 1/3rd of reporters have to double-report for 2 outcomes to seem valid. You need 2/3rds for an outcome to seem valid)

2) we need a contract for oracle reporters to commit to their judgment, so that they have to reveal it later, and can't change their mind. Reporters make this commitment to the leader.

3) We need a contract so that if a majority of oracle participants commit in the same direction, those commitments can be used to unlock a payment. The gamblers will use this contract with the leader.

4) We need a leader to commit to deciding who the honest oracle participants are. He needs to commit to making this decision exactly once. When he does decide, this distributes a large amount of money as fees to the honest oracle reporters.
This confiscates the safety deposit from the dishonest reporters. The confiscated money is split between the gamblers, and the honest reporters.
We need to pay gamblers, because otherwise the reporters and leader could collude to rob the gamblers.
We need to pay the honest reporters to incentivize any colluding attackers to have a sub-group of counter-attackers.
This contract is between the leader and the reporters and the gamblers.

