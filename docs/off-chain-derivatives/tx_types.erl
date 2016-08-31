commit to oracle- a commitment to give the answer to a binary question by a certain point in time. Can have a hashlock. Can lock more than the minimum required amount of money.
oracle start- commits to revealing a certain fact that is written in this tx, this can be receded by commit to oracle, or not.
oracle- this is the tx that validators can censor if they dislike. it must be preceded by oracle start.

everyone-signs- everyone who is a validator signs this tx. only the merkle root is written on the tx.
reveal-signature- uses random number to select a random set of validators who we need proof that they signed the merkle root.
