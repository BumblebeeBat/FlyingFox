The oracle could be a single person who makes a big safety deposit. the safety deposit is locked up long enough that the coin-holders can work together to agree that he lied and destroy his safety deposit.

Since we can't store all those signatures on a single block, we use this process:
1) post to the blockchain the merkle root of the patricia tree containing all the coin-holder agreements.
2) wait for the blockchain to generate a random number, and use that number to decide which account's signatures matter.
3) if enough of these accounts agreed that the oracle lied, then the oracle loses its deposit.




the oracle could be a central manager (or N of M multisig of managers) who everyone makes a channel with.
Everyone makes contracts with the manager, they agree to report on the outcomes of decisions on certain dates.

When the oracle makes his decision, he sets the nonce as high as it can go. So he can't update the channel again, and he has to submit it as the final state.