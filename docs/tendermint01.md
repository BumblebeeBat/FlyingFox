Tendermint is unprepared for this failure mode:

Block number N is signed by validators and put onto the blockchain. Someone is very regretful about block N, they wanted it to be different in some legal way. They are willing to pay more than all the safety deposits to have block N remade. So they pay the validators a big bribe, and the validators sign a second block at the same height.

Jae and Ethan agree that a hardfork is the only way for Tendermint to recover.
Then Ethan changed his mind, and decided subjectivity is the way to recover.

Subjectivity can't necessarily recover from this failure mode because:

We can't assume that validators will only grow one side of the fork. Each side is incentivized to keep it's own side alive.
We also can't enforce the rule: "oldest fork is correct".
Because the younger fork might be willing to donate a large amount of money to the validators, or to burn a large amount of money.
If the majority of users can benefit by breaking the rules, then a majority party will come to consensus to break the rules.
They will hard fork to make the younger fork survive, in which case both sides would survive, which is a failure mode.

Hard forks can't necessarily recover from this failure mode because:

We would need decentralized process to decide which state we are going to hard fork to. This decentralized process would need to be resistant to forking.
Any decentralized process resistant to forks that can come to consensus about state is called a "blockchain".
So, for Tendermint to recover using a hard fork, it would have to be dependent on another blockchain like bitcoin.
If the bitcoin miners choose the side that harms the majority of Tendermint users, then a majority of Tendermint users will do a hard fork to keep the other side alive. Both sides would live, which is a failure mode. So we can't depend on another blockchain either.

The only mechanism I am aware of that stops failure modes like this is:

there are 2 people who want different legal state transitions, and they are willing to spend more than 1/3rd of the safety deposits, then both forks survive for a time. The forks fight to the death. Whichever fork is willing to burn more money wins.
The longer the fork lasts, the more money needs be deleted exponentially. For the fork to last as long as finality, you would need to delete all the money on the chain in both forks.
The users of Flying Fox prefer the chain that had more money deleted, because they own a larger portion of the money.
The value-holders are incentivized to let the wrong side die. 