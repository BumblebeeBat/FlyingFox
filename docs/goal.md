Flying Fox Goal

A channel is when 2 people put their money onto a small blockchain for a while. Creating the channel costs money, but once inside the channel it is free to do everything. Inside the channel the 2 participants can make any sort of bet with each other, and they can send each other money back and forth.

When channels are used for betting. The participants give a yes/no question in ascii text, and record the hash of the question in the channel state. In the event of a disagreement about a state transition in a channel, the previously agreed upon channel state gets published to the blockchain, and becomes part of the blockchain state. Then they give the ascii bet to the oracle.

When channels are used for betting, it needs to be very secret. In most cases, the only people who read the text of what is being bet on are the participants in the bet.

Flying fox has questions in ascii instead of a scripting language, because each oracle might have different rules about what types of questions they support. Many will use scripting languages, others will use various human laguages.

Example of playing a board game:
Say 2 people want to play a board game. They write the rules of the game in english, or some programming language. They update the channel state to contain the hash of the rules. They take turns updating part of the channel state that encodes the current position of the game pieces. If someone refuses to play correctly, the other person can publish the channel state onto the blockchain, and also share the rules with the oracle participants. The oracles verifies that the rules match the hash that both player signed, and uses the rules to determine how the game money should be divided up.
In most cases playing the game will not involve any blockchain transactions at all. The entire game is in channel.