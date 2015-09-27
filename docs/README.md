flying fox docs
=============

Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT


I give thanks to some people who contributed ideas in the influenced_by page, and I link to a lot of essays that explain how parts of flying fox work.

goal.md gives a non-technical explanation of what is being made.

outline.md gives an overview of how the project works. It explains about blocks, and it explains each tx type.

glossary.md is a list of words useful for understanding this project.

channels.md is a spec for channels that is supposed to be good enough that you could implement channels on another blockchain after reading it.

failure_modes.md explains 3 types of attacks that can be conducted against proof of stake, and compares flying foxes defences to existing byzantine fault tolerant ideas especially tendermint.

security.py is where I calculate that 1/2 need to be byzantine in order to successfully attack.

stablecoin has notes on how to use market mechanisms to create coins that maintain constant value against an external asset. Including a scripting system that allows customized financial contracts without a turing complete scripting language.

SMPC.py explains a secure multi-party computation mechanism that will be used to maximize how often the oracles give accurate answers.

The rough drafts directory is the remains of old versions of Flying Fox. 

undecided_pieces is a list of changes I have not decided on yet.
