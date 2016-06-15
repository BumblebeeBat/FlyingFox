Calculation of how much more efficient a blockchain can be if it splits its bonds into 2 types.

Security efficiency number is important because, for any blockchain consensus protocol, doubling spending on security should double the cost of an attack.
When we compare cars, we look at miles per gallon, called "fuel efficiency". How much you have to spend to run the car.
When we compare consensus protocols, we look at security efficiency. How much you have to spend to spend to run the consensus protocol.
The security efficiency of a blockchain is (cost to do a soft fork attack)/(cost to maintain consensus for a day).

Blockchain A has 1 type of bond. 1 >= X > 0.
where X is the portion of the value on the blockchain that is in bonds.

Blockchain B has 2 types of bonds. 1 > L > S > 0, L+S <= 1.
Where L is the portion of the value on the blockchain in the big bond.
and S is the portion of the value on the blockchain in the small bond.
In the case of Flying Fox, L / S = 128

The interest rate is R.
The operating costs for blockchain A are X * R.
The operating costs for blockchain B are S * R.
The operating costs for blockchain A are X / S times bigger than the operating costs for blockchain B.

The cost to attack blockchain A with a soft fork is as much as buying 2 * X / 3 of the coins.
The cost to attack blockchain B with a soft fork is as much as buying 2 * L / 3 of the coins.

The cost to attack A with a soft fork is X / L times bigger than the cost to attack blockchain B with a soft fork.

The security efficiency of blockchain A is 2 * X / 3 / (X * R) = 2 / (3 * R)
The security efficiency of blockchain B is 2 * L / 3 / (S * R) = 2 * L / (3 * R * S)

The security efficiency of blockchain A is L / S times smaller than blockchain B.

In the case of flying fox, L / S = 128, so the 2-bond blockchain has 128 times higher security efficiency than the 1-bond blockchain.

So, given the same operating costs, Flying Fox can secure a market cap that is 128 times larger than bonded proof of stake systems that only have 1 type of bond.

[explanation for why 2 types of bonds is so much more efficient](2_types_of_bonds.md)

