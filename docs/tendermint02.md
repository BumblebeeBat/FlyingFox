Tendermint is unprepared for soft fork attacks.

It costs 2/3rds of the validating power to get the ability to censor any transactions. The ability to censor transactions is the same as the ability to steal any money on the chain.
Since the amount you can steal is bigger than the cost of committing the attack, Tendermint is vulnerable to this attack.

The only mechanism I am aware of to stop this kind of attack is like this:
The distribution of people who can cause soft forks should be as similar to the distriution of value holders as possible.

The Tendermint team is planning on using a security deposit delegation mechanism to stop this type of attack.
Security deposits are an excessively expensive solution to the problem. Flying Fox only needs <1% of the money bonded at a time. A security deposit delegation mechanism would lock up >50%. The cost of security deposits is the interest rate. So Tendermint would be >50x more expensive than Flying Fox.

Delegation mechanisms have another problem: If the cost of being a validator isn't in bonding coins, then the validators wont necessarily own much stake at all. In which case we are vulnerable to retirement attacks.

