flying fox docs
=============

Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT

I give thanks to some people who contributed ideas in the influenced_by page, and I link to a lot of essays that explain how parts of flying fox work.

outline.md gives an overview of how the project works. It explains about blocks, and it explains when each tx type is valid, and it explains how each tx type changes the state.

failure_modes.md explains 3 types of attacks that can be conducted against proof of stake, and compares flying foxes defences to existing byzantine fault tolerant ideas especially tendermint.

security.py is where I calculate that 1/2 need to be byzantine in order to successfully attack.

stablecoin has notes on how to use market mechanisms to create coins that maintain constant value against an external asset. Including a powerful scripting system that allows highly customized financial contracts without a turing complete scripting language.

patricia is a database structure useful if we ever want computers to interact with the blockchain without downloading the blockchain. I may add it in the future.

SMPC.py explains a secure multi-party computation mechanism that will be used to maximize how often the oracles give accurate answers.

The python directory is the remains of an old project which I use for reference when making flying fox. 

todo is a list of changes I am considering doing to flying fox source.