trigle: leader, oracle reporters, gamblers.








A common pattern that comes up is a triangle of channels between 3 nodes. One channel is supposed to have some money deleted.
The node opposite this channel needs that money to be deleted, or they wont be secure.
The third node needs a way to make sure the other 2 nodes actually delete the money.

new tx type: delete from channel.
both channel participants sign it.
It makes their channel have less money left.
It includes a hash which needs to get kept in a database for a while.

new tx type: punish channel.
If you can prove that 