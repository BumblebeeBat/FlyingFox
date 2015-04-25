Comparison of failure modes between flying fox consensus and traditional byzantine fault tolerant consensus (like tendermint or hyperledger).

There are 3 failure modes I examine that can happen to proof of stake blockchains: bribe, forking, and halting. 
Bribe failure mode is when some change of consensus state is highly contested, and coin-holders pay money to redo history, possibly repeatedly. 
Forking is when there are more than one valid looking histories existing at the same time. Maybe half of the network follows one, and half follows the other. 
Halting is when a large percentage of validators disappear, and it becomes impossible to continue the blockchain.

Overview of 2 consensus strategies, traditional byzantine fault tolerance, and flying fox:

In byzantine fault tolderant consensus systems that I have seen, the programmers act as dictators for all these problems. If anything goes wrong, the programmers issue a new version of the protocol which starts before the error has occured. The programmers have a tendency to choose the history that benefits them personally. If the programmers disagree and release multiple new versions, the forks can persist this mechanism.

Flying fox is a more economic protocol. It tries avoid relying on humans as much as possible. Eventually flying fox will have a last update, after that point in time we can start teaching flying fox to machines which are too costly to update later. Flying fox tends to give more control to users willing to burn more money. So the cost of fighting tends to go to the users participating in the fight, and no one else.

1) the blockchain should freeze, and need to be manually restarted by programmers who release a new version of the protocol which starts before the error had occured.
*VSUs will have trouble updating for the new protocol.
*The programmers have a ton of power every time they fix an error. 
*In many cases neither history is more provably accurate than the other, so the programmers can choose whichever benefits them personally. 
*The attacker could bribe the programmers or the programmers and the attacker could be the same person. 
*If the programmers disagree, then multiple versions of the protocol could be released, and the users wont know which version to download.

2) Allow the users who create blocks to simultaniously burn coins. In the event of a fork, everyone should continue mining on the fork that cumulatively burned the most money in it's history.
*an attacker who is willing to burn his own money can simultaniously double-spend a smaller amount of money. He has to burn at least 1/3rd of all the security deposits for each block he destroys, plus block_fee*2^(number of blocks being destroyed), plus an amount of money equal in size to the victim's loss. Just like in bitcoin, if the victim waited for more confirmations, then their money becomes more and more secure against attacks like this.
*When money is destroyed it is deflation. The rest of the money becomes more valuable.

