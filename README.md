Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT

Flying Fox is my attempt to achieve the goals of the truthcoin project: http://www.truthcoin.info/ I respect Paul Sztorc, especially in his ability to communicate truthcoin, which he invented. Him and I are using very different strategies to try and build the same thing.

Sztorc maintains a forum where the different teams trying to build truthcoin can share ideas: http://forum.truthcoin.info/

Flying Fox deviates from Sztorc's design choices in several ways:

*Not based on bitcoin, it is a new blockchain with a new consensus mechanism optimized for scalability.

*Does not use bitcoin as the internal currency for betting, instead Flying Fox will be a seperate cryptocurrency.

*Finite memory requirement.

*An Erlang/OTP package, because it is a lot easier to verify that erlang code is secure in comparison to C++.

*Uses lightning network channels for betting. 

*Judgement on the outcome of bets occurs off-chain, in the lightning network.

*Proof of stake blockchain where each peer's power in the consensus process is determined by how much money they have locked in channels.

Code of conduct, development guidelines: Submit pull requests. If I like, and it passes tests, I will accept. Feel free to ask me questions or make github issues.

Install instructions:

Eventually Flying Fox will be a simple executable you click on. For now it is a bit more complex.

First, make sure you have erlang installed. Version 18 is prefered, but older versions will probably work. Here is one way to download it: http://www.erlang.org/download.html , then run the script.

sh install.sh


Start your node with this script:

sh start.sh

Then open this URL in your browser: http://localhost:3011/main/