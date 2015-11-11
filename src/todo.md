messanger system that lets you pay to send messages to peers with commands.

To make hashlocked tx with scripts more affordable, we should let bet reveals reference blocks that were revealed since max_reveal. That way you don't have to reveal the same data onto the chain twice for a 2 step hashlocked lightning.

we need an arbitrage module that keeps track of which bets are identical in multiple channels. If a new way of unlocking the bet is revealed on one side, we put a message in the other parties message box to update our channel.

The network is make up of users, who are rarely on, and the hubs that are always on. When a hub wants to complete a lightning transaction to a user, it drops the message in the user's mailbox, and waits for the user to pull the data with an http request. When 2 hubs want to complete a lightning transaction, they need to push the message to the next hub as quickly as possible. Don't wait for the other hub to request the data.

hashlock lightning payments

update download_blocks so that we can reach consensus with a node that is further in the future than finality.

