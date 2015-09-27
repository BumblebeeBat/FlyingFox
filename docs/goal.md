Flying Fox Goal

This is supposed to give an idea of how Flying Fox is typically used.

Like bitcoin, you have an address, and you can recieve money at your address, or spend money to other people's addresses.

Once you make a channel, you can bet. The participants give a yes/no question in binary, and record the hash of the question in the channel state. In the event of a disagreement about a state transition in a channel, the previously agreed upon channel state is given to the oracle to judge on. After the oracle writes his judgement on the tx, if the tx is published to the blockchain, then the oracle's decision determines how the money is split between the gamblers.

When channels are used for betting, it can be very secret. In most cases, the only people who read the text of what is being bet on are the participants in the bet. If one participant tries to reveal, the other participant can reveal a private key, and make all their signatures on the bet deniable. It uses off-the-record encryption.

Flying fox has questions in binary instead of a scripting language, because each oracle might have different rules about what types of questions they support. Many will use scripting languages, others will use various human laguages.

Example of playing a board game:
Say 2 people want to play a board game. They write the rules of the game in english, or some programming language. They update the channel state to contain the hash of the rules. They take turns updating part of the channel state that encodes the current position of the game pieces. If someone refuses to play correctly, the other person can publish the channel state onto the blockchain, and also share the rules with the oracle participants. The oracles verifies that the rules match the hash that both player signed, and uses the rules to determine how the game money should be divided up.
In most cases playing the game will not involve any blockchain transactions at all. The entire game is in channel.