Eventually we want to make things like SMPC and Market Makers to complete the truthcoin project, I want to make games like poker too.

How do you decide if it is cost-effective to use sidechains, or channels for your contract?

N = The number of participants in your contract times the maximum amount of money a participant could recieve from the contract.

If you use channels, then you will need a loan for at least the amount of money you could win for the entire duration of the contract. This can be cost prohibitive for some applications. The loan is money on the opposite side of your channel, so that if you win you can get paid. It is a liquidity problem.

Flying Fox will not support sidechains in the near future.

I imagine each SMPC as a team of several validators, and 1 central node who has a channel with each of them. The central node's relationship is trustless to everyone else. The validators keep using hash-locked transactions to trustlessly update their channel-states with the central node. 
The central node keeps track of the hashes of all the secrets so that we can tell who is making mistakes and remove them.