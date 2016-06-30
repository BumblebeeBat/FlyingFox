abstract: We show that shard scaling is insecure against soft-fork attacks, and that state-channel scaling is secure.

Sharding is contradictory with this goal: money inside of baby chains should give it's owner just as much power over consensus as the same amount of money on the main chain.

N is the number of transactions made. Without scaling O(N) is how many transactions need to be processed by every node. With scaling O(log(N)) is how many transactions need to be processed by every node.

The scaling methods being researched now look like many baby blockchains that are linked to one big one. The baby blockchains have many transactions, but only rarely report their state to the main chain.

There are 2 major strategies for scaling. Some people think that the baby chains should have exactly 2 users each, this strategy is called "state-channels". Other people think that the baby chains should have any number of users each, this strategy is called "shards".

Let us examine scaling in the context of soft-fork attacks. [there must exist a distribution of users who control soft forks. This is the same distribution that decides which block to add to the chain next.](2_types_of_bonds.md)

Since we only want to process O(log(N)) transactions on the main chain, we need the percentage of value on the main chain to be a tiny fraction of the total money in the system.
As the number of baby chains increases, the portion of value on the main chain must keep decreasing.

To defend against soft-fork attacks, the distribution of users who can cause soft forks should be as similar to the distribution of value holders as possible. The users who hold value on baby-chains should have just as much power over soft forks as users holding the same amount of value on the main chain.

The main chain knows the total amount of value on each baby.
In the case of state-channels, the main chain knows the users on each baby.
In the case of shards, the main chain doesn't know the users on each baby.

We need to use delegation so that the baby chains can have some control over adding blocks. If their delegate acts inapropriately, the baby chains will elect someone different.

In state-channel scaling, it is possible to require that the delegate elected by a baby-chain needs to be an invested member of that baby chain. State channels only have 2 members who are both well invested in the channel relationship, either by money locked into the channel, or by paying rent to keep the channel open. Delegates earn power by investing in the blockchain. So the distribution of users who control soft forks would be very similar to the distribution of value holders.

In sharding scaling, the parent chain doesn't know who holds money on the babies. So it is not possible to require delegates to be invested in the blockchain. Delegates win their power by campaigning, not by investing in the system. So the distribution of users who control soft forks can be very different from the distribution of value holders.