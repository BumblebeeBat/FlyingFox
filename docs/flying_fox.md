A cryptocurrency with Turing complete smart contracts.

Innovations:

* lightning network allows for money to be sent instantly, and for smart contracts to be deployed instantly. You don't have to wait for any confirmations.

* control over adding the next block is based on how much money you have instead of how much money you are willing to lock up. So power of choosing the next block is in the hands of users rather than a special elite class.

* Achieves the "useful proof of work" goal. The value that gets destroyed maintaining consensus is simultaniously being used to improve the lightning network. Lightning networks can be expensive because a lot of money is locked up, this cost is passed to the users in the form of transaction fees and rent. Lightning consensus covers some or all of this cost, so you can send money with lower or zero fees, and your rent cost is lower or negative.

* Flying Fox is the first implementation of state channel blockchain scaling. Which requires less assumptions than sharding style scaling. Sharding is experimental, we don't know how to build it yet. Channels are simple and understood.

* All of this has been successfully implemented in the erlang language as a part of Flying Fox. 