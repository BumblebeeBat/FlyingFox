Blockchain A has 1 type of bond. 1 > X > 0. X of all the value of the blockchain is in bonds.

Blockchain B has 2 types of bonds. 1 > L > S > 0, L+S < 1. L of all the value in the blockchain is in the big bond. S of all the value on the blockchain is in the small bond.
In the case of Flying Fox, L/S = 128

The interest rate is R.
The operating costs for blockchain A are X*R.
The operating costs for blockchain B are S*R.
The operating costs for blockchain A are X/S times bigger than the operating costs for blockchain B.

The cost to attack blockchain A with a soft fork is as much as buying 2*X/3 of the coins.
The cost to attack blockchain B with a soft fork is as much as buying 2*L/3 of the coins.

The cost to attack A with a soft fork is X/L times bigger than the cost to attack blockchain B with a soft fork.

The security efficiency of a blockchain is (cost to do a soft fork attack)/(cost to maintain consensus for a day). This number is important because in general doubling the spending on security should double the cost of an attack. When we are deciding how much to invest in securing the blockchain per day, we use this number.

The security efficiency of blockchain A is 2*X/3/(X*R) = 2/(3*R)
The security efficiency of blockchain B is 2*L/3/(S*R) = 2*L/(3*R*S)

The security efficiency of blockchain A is L/S times smaller than blockchain A.

In the case of flying fox, L is 128 times bigger than S, so the 2-bond version of Flying Fox is 128 times higher security efficiency than the 1-bond version.

So, for the same operating costs, the 2-bond version of Flying Fox can secure a market cap that is 128 times larger.

[explanation for why 2 types of bonds is so much more efficient](2_types_of_bonds.md)

