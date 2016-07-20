There are 2 ways the trading is being designed on blockchains today.

The naive choice looks like this:
trading is broken up into multiple independent interactions with the blockchain. Creating the market,
trading,
 closing the market.
The blockchain needs to remember the state of a market between all these steps.
This choice involves writing code into the consensus protocol to manage the state of the market.
Market scoring rule,
and the auctioneer,
 and the oracle consensus would be written into the blockchain consensus protocol. So none can be updated later.

The correct choice looks like this:
reporting on the outcome of trading is a single interaction with the blockchain that occurs between a pair of users. Creating the market,
trading,
 and closing the market all happens as one step.
The blockchain doesn't need to remember anything about the state of the market.
This choice adds a couple words to the scripting language, very little code needs be added to the blockchain consensus protocol.
The market scoring rule, the auctioneer, and the oracle consensus protocol would be written using the scripting language, so we can update them later.

Maintaining a line of code in the blockchain consensus protocol is about 100x as expensive as maintaining a line of code in the scripting language.