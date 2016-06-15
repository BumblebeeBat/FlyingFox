There are [2 types of bonds](2_types_of_bonds.md) in Flying Fox.

How do we determine the optimal ratio between the big one, and the small one?

Here are factors that depend on the size of these bonds:

* How long it takes for all the money on the chain to change hands.

* The efficiency of the blockchain = (The cost to attack the blockchain) / (The cost to maintain defenses for one day for the blockchain)

We want to be able to move all the money on the chain within 24 hours. So `(Small Bond) * (blocks per day) / 3 > (market cap)`.

In ethereum's case there are about 350000 blocks per day so:
 `(Small Bond) > (market cap) / 120000`

So this limitation is a lower limit on how small the bond can be to accomplish our goals.

The efficiency of the blockchain grows as `(Big Bond) / (Small Bond)` which is calculated [here](2_types_of_bonds_calculation.md)

So for efficiency's sake we want to have as small of small bonds as possible, and as big of big ones as possible. When we combine these 2 equations:

`(Big Bond) / (Small Bond) = 11999`
