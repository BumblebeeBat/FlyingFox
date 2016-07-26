In Casper PoS, if colluding attackers ever had 67% of the staked coins, then they would have complete control over deciding which block is added next.

If the other validators are unaware of his advantage, the attackers could use the casper betting mechanism steal all the coins that are at stake.

For example, if there are 2 alternative next blocks, either A or B.
The attacker with 67% could wait for the other 33% to choose A, and then the attacker would choose B. Which would either take or destroy all the coins staked on A.

Once the attacker has this level of control, he can start to sell off his coins while censoring anyone who tries to put their coins at stake.
So the attacker will own <50% of the coins, while having 100% control over the consensus.
The network would probably use a hard fork to recover from an attack like this, but it will be difficult. If the attacker used profits from attack to pay someone coins before anyone knew the attack was happening, should we take away those coins too?
There is not necessarily a state we can all agree to recover to, so the network wont necessarily be able to recover on a single blockchain. After we recover from the attack, there might be 2 or 5 or 20 versions of Casper that come out of it.

Tendermint has an alternative mechanism to consensus by bet. It involves 3 rounds where the validators come to agreement to all sign on the same block. Tendermint's mechanism solves the same problems as consensus by bet, but without the possibility of this terrible failure mode.
