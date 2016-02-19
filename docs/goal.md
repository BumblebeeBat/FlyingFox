Flying Fox Goal

This is how Flying Fox is typically used.

Like bitcoin, you have an address, and you can recieve money at your address, or spend money to other people's addresses.

Once you make a channel, you can bet. The participants make a yes/no program, and record the fingerprint of the program in the channel state. If there is a dispute, the output of the program controls who gets the money.

When channels are used for betting, it is very secret. The only people who read the text of what is being bet on are the 2 gamblers.

Flying fox has questions in binary instead of a scripting language, because each oracle might have different rules about what types of questions they support. Many will use scripting languages, others will use various human laguages. For example, an oracle made up of Mandarin speakers will accept bets written in Mandarin. The American oracle will accept bets in English.

Example of playing a board game:
Say 2 people want to play a board game. They write the rules of the game in english, or some programming language. They update the channel state to contain the hash of the rules. They take turns updating part of the channel state that encodes the current position of the game pieces. If someone refuses to play correctly, the other person can publish the channel state onto the blockchain, and also share the rules with the oracle participants. The oracles verifies that the rules match the hash that both player signed, and uses the rules to determine how the game money should be divided up.
In most cases playing the game will not involve any blockchain transactions at all. The entire game is in channel.
This is a truthcoin style oracle http://www.truthcoin.info/