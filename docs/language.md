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

Code would mostly be integer codes like:
0, 1, 5, 30
But it would have Things mixed in:
<<>>, {f, 3, 2}, {integer, 33}

I could have the datastructure be a list of Things,
examples of Things:
* <<>>
* <<123, 44, 0, 1>>
* {f, 23, 100} % fraction.
* {f, 0, 1}
* {f, 230000000, 1}
* 27
* 0
* true/false

The code could be a list of Things and Opcodes. Things get pushed to the stack.

The bet is a list of bytes in the language. The key to unlock the bet and find it's outcome is another list of bytes.
Checking find the outcome of the bet is as easy as appending the bet to the key, and run the bytes through the VM.
When the script finishes, the output looks like:

[ X, Y, Z, ...]

Where X is the top of the stack.
X is the portion of the money that gets deleted, between 0 and 1.
Y is the portion that goes to player 2, between 0 and 1.
Z is a nonce. Only the highest nonced output possible is valid, otherwise your partner could slash you.

If there is a stack underflow, or any other error processing opcodes, then it is an invalid tx.