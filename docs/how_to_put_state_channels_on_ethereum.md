How could we put state channels onto ethereum? 3 possible methods analyzed.

One option is to write a Channel-VM in EVM, and use the CVM for the state in state-channels. Multiple layers of VMs tends to be inefficient.

A second option is to build the channels natively, and reuse EVM as the VM.

State channels operate like bitcoin scripts. There is some code that both participants agree upon, called the scriptpubkey, and there is code provided by the participant who is closing the channel, called the scriptsig.
When a channel is closed, the scriptsig is appended in front of the scriptpubkey to make a valid script, which is fed into a VM.
We are considering how well EVM can be used for the VM of a scriptsig/scriptpubkey system.
The major problem is the Jump opcode. As EVM works now, someone could jump from the scriptsig over most of the scriptpubkey.
So we need to disable the Jump and Jumpi opcodes during the scriptsig portion of the script, only allowing them during the scriptpubkey portion.

This is an unfortunate limitation, because it means we need to define all the functions in the scriptpubkey portion of the script, which increases bloat, and decreases privacy, and increase cost to close a channel. Every time a channel is closed, all of it's code would get published.

Flying Fox Forth, the scripting language I made for my research doesn't have this limitation. In FFF you can refer to undefined functions in the scriptpubkey, and define them in the scriptsig.
Each function is named by the hash of it's contents.
So in FFF, when a channel is closed you only reveal the code that gets executed to close the channel. Any alternative paths that are unexecuted don't get published to the blockchain.

A third option is to include a second VM in ethereum besides the EVM, and use this alternative VM for state channels. Each VM is expensive in terms of maintenance and software rot.