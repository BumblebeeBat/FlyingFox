Sharding does not work well with the 2 types of bonds plan.

The people who have power over soft-forks need to be sufficiently invested in the system so that they will make decisions that are in the best interest of the system. 

In the event of a soft-fork attack, we care about this ratio:

`(how much value the validators lost) / (how much value was destroyed on the chain)`.

Ideally this ratio should stay as close to 1 as possible, that way the attacker is only able to destroy as much money as he pays to execute the attack.

An upper limit on that ratio is this ratio:

`(how much value the validators control) / (how much value is on the chain)`.

The bond needs to be very big. Bigger than 9/10ths of the market cap hopefully.

If we move a significant portion of the value into a shard, then that value cannot be a part of this bond.

So it will be too cheap to bribe validators to break the blockchain.

So we should consider using state-channel scaling instead.