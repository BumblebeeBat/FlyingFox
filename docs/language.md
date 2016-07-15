The language is turing complete because it allows recursive functions.

The language uses forth syntax.

[The list of opcodes](/language_opcodes.md)

The language is similar to bitcoin in that it uses scriptSig and ScriptPubKey https://en.bitcoin.it/wiki/Transaction

The scriptPubkey is a list of Flying Fox Forth (FFF) opcodes.
The scriptSig is a list of Flying Fox Forth (FFF) opcodes.
The ScriptPubkey is signed by both participants of the channel.
The scriptsig is signed by the participant who is trying to close the channel.

The scriptSig is appended in front of the scriptPubkey to make the code that is given to the FlyingFox Virtual Machine (FVM).


the scriptsig portion isn't chosen until the last possible moment. That is the portion where we want to make a lot of decisions at the last moment. We will know which branches of the script will get executed. We will know which functions get called.
This is the moment when we should be making the decisions about which functions to define. It is best if the functions are defined in the scriptsig. 