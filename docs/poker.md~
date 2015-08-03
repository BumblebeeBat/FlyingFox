imagining a game with 9 players. Each player makes a channel with a central node. They agree upon a judge.

So long as none of the players have modified the software, and the central node uses normal software, they will never actually need the judge. The computer will only let you play poker normally.

You are really only transacting with the central node, not your peers. So if you and the node disagree on your current balance, then the node will refuse to sign the next channel-state.
If the node is lying to hold your money ransom, then you can get your channel block signed by the pre-agreed upon judge. The judge writes down how much money each of you would get if the game were to end right now.

If the node still refuses to sign the next channel block, then you publish the channel block the judge had signed to the blockchain, along with a part of the channel-state I call the conserved quantity. The conserved quantity is used to protect the node from the judge.

Attack: What if the judge signs in conflicting ways on each channel to steal all the node's money?
It can't happen because when the judge signs one of them, because of how the conserved quantity works, he has really signed all of them at that moment.
If any one of the channels were to get published, then the judge has the option of ending all 8 other channels at the same state. If all 9 get closed, then the nodes losses and wins cancel out, so the node never has to trust the judge

Instead of programming in a turing complete language like EVM, all we need is a simple language for describing conserved quantities between groups of addresses.