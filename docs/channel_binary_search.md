This paper presents a simplification to the binary search channels.

The idea of using binary search to simplify the verification of channel contracts is very clever. If a channel contract is N opcodes long, the blockchain only needs to compute log(N) opcodes. It goes like this:

Alice and Bob make a channel, and put a contract in their channel.
Alice and Bob disagree about the outcome of their contract.
They tell the blockchain to start the process of closing the contract under disagreement.
First the blockchain asks for the VM state at the point when it is half way through processing the contract.
If Alice and Bob agree on the state at that point in time, then the blockchain knows the error must be in the second half of the contract.
If Alice and Bob disagreed on the state at that point in time, then the blockchain knows the error must be in the first half of the contract.

By repeating this process, the blockchain eventually identifies which opcode was processed incorrectly, and which of Alice or Bob messed up.


Here is my simplified protocol to achieve the same goal:

Alice and Bob make a channel, and put a contract in their channel.
Alice and Bob disagree amout the outcome of their contract.
They want to put it onto the blockchain to be judged, but they also want to save money. If the contract is too long, it costs more.
So Alice and Bob talk to each other about which parts of the contract they disagree about.
They show each other the VM state at the point when the VM is half way through processing the contract.
If they agree on the state at that point in time, then they can get rid of the first half of the contract.
If they disagree on the state at that point in time, then they can focus with more granularity on the first half of the contract to find their disagreement.

By repeating this process, Alice and Bob eventually identify the shortest possible contract that they have disagreements about. This tiny contract gets published to the blockchain.


Benefits of this simplified protocol:
Instead of needing to publish O(log(N)) tx to the blockchain (where N is the length of the original smart contract), Alice and Bob only need to publish O(1) tx to the blockchain.
