Don't Reuse Bonds in Proof of Stake

Most proof of stake systems use the security bonds, the "stake", for 2 unrelated goals.

1 Ownership of stake determines how much power you have in deciding the next block of the chain. This group has the power to make soft forks, which can change the consensus mechanism in every way. The people who have power in deciding the next block need to be sufficiently invested in the system so that they will make decisions that are in the best interest of the system.

2 If validators should double-sign, the stake gets destroyed to punish them.

2 To protect against double-signing, the bonds need to be locked up an innaccessible for a long time. This is expensive by the interest rate on the locked up funds, but since the bond can be relativly small like 1/10000th of the market cap, we are fine.

1 We need the validators to be well invested in the system so that it is very costly to make poor decisions.
A ratio:
how much value the validator needs to lose to commit a malicious soft-fork attack
/
how much value gets destroyed on the chain.

Ideally this ratio should stay as close to 1 as possible, that way the attacker is only able to destroy as much money as he pays to execute the attack.

An upper limit on that ratio is:
how much value the validators control
/
how much value is on the chain


The bond needs to be very big. Bigger than 9/10ths of the market cap hopefully. Luckily this bond doesn't have to be innaccessible. We can use DPOS or mix this bond with the channel mechanism so that the money is spendable all the time.


If we use the same bond to accomplish both (1) and (2), then it will do both things poorly. It will be too big for (1), and it will be too small for (2).