This is a malicious soft fork attack that can be done against any blockchain with payment channels.
The validators make lots of channels with lots of people.
The validators make channels where they used to own lots of money, but now own little money.
The validators close all the channels at the wrong point in history at the same time.
Users are all trying to provide evidence to get their money back.
There isn't enough space for everyone to provide evidence. Users are willing to pay high fees to get their evidence into the blockchain.
Validators profit from both fees, and from channels that close at the wrong state.

This problem can be solved with the addition of a new tx type.
channels need 16 or 32 more bits of memory.
The participants of the channel need the ability to sign a tx committing that they will only close the channel at a nonce bigger than some limit.
So, if I used to own only 10% of the money, but now own 90%, I would want to lock in my higher balance, so there is no way for you to close the channel at the low balance.
Since this tx is so small, and it doesn't transfer money, it should be affordable.
