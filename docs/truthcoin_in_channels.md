How do we put truthcoin into the channels mechanism?

1) We need a contract so that oracle participants can commit to judging exactly once. The sum of safety deposits provided by oracle participants is the limit for how much money can be gambled in this market.
2) We need a contract so that if a majority of oracle participants commit in the same direction, those commitments can be used to unlock a payment.
3) We need a leader to commit to deciding who the honest oracle participants are. He needs to commit to making this decision exactly once. This distributes a large amount of money to the honest oracle participants.
4) It must