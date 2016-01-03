It is amazing to me how prolog appeared in Flying Fox. It was not planned. Ill try to walk you through how it happened.

bitcoin script is the non-turing complete forth-like language invented for bitcoin.

flyingfox script is the non-turing complete forth-like language invented for flyingfox. 

ScriptSig is from bitcoin. It is the stuff you write onto a transaction when you sign a transaction. It contains a cryptographic signature. It is written in bitcoin script.

ScriptPubkey is from bitcoin. It is the lock on the transaction that expains what you have to do to spend from it. It is written in bitcoin script.

paytoscripthash was a strategy that I learned about in the bitcoin project. It is used so that the scriptSig contains the code instead of the scriptpubkey. The scriptpubkey only contains a hash of the code.

Putting code on-chain to process is expensive. The code needs to be loaded onto every node in the network. We want to make the code that goes on-chain be as small as possible.

In an effort to keep the on-chain code small, flying fox has a feature similar to paytoscripthash. Instead of a single hash, it has an entire merkle tree of hashes. That way you can unpack the portion of the code that you need for the situation you are in.

Now for a couple examples.

If the game of goofspiel was written in flying fox, and we are part ways through a game so that your only cards left are ace, duce and jack, the rules would look something like this:

* You can't play between 3 and 10
* you can't play <1
* you can't play >11
* you can't play non-integer
* whoever plays the highest card wins.

If your partner illegally played a 4, then you would use a merkle tree to prove "You can't play a 4", and you would use the signed "4" from your partner, and then you could prove that your partner cheated.

If your partner played a 2, and you played a 4, then you could take both the signed "2" and the signed "4" and the merkle proof of the rule "whoever plays the highest card wins". Then you could claim your prize.

This process was designed to minimize cost of programming on the blockchain. It just happens to result in a prolog-like language. So that is why flying fox script is compiled from a prolog-like language.