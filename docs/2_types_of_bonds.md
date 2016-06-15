Don't Reuse Bonds in Proof of Stake

Most proof of stake systems use the security bonds, the "stake", for 2 unrelated goals.

(1) The first goal is to stop consensus forks. If validators should double-sign, the stake gets destroyed to punish them. In order to be able to punish, the bonds need to be locked up and innaccessible for a long time. Locking up funds is expensive by the interest rate. Ethereum and Tendermint each lock up about 1/20th of the coins, so these interest rate costs are very expensive. In Flying Fox the bond can be relativly small, like 1/5000th of the market cap, so the interest cost on this bond is about 250 times less than ethereum or tendermint.

(2) The second goal is to stop bad soft forks. Ownership of stake determines how much power you have in deciding the next block of the chain. This group has the power to make soft forks, which can change the consensus mechanism in every way. The people who have power over soft-forks need to be sufficiently invested in the system so that they will make decisions that are in the best interest of the system. 

The bond needs to be very big. Bigger than 9/10ths of the market cap hopefully. Luckily this bond never gets slashed, so it can be accessible all the time. 

If we use the same bond to accomplish both (1) and (2), then it will do both things poorly. It will be too big for (1), and it will be too small for (2).

[calculation of exactly how much more secure having 2 types of bonds makes us](2_types_of_bonds_calculation.md)

DPOS is one way to solve this problem.
Flying Fox mixes the delegation mechanism with the channel mechanism, which is another way to solve this problem.