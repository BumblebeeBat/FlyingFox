Flying Fox
==========

Work has been moved to Amoveo: https://github.com/zack-bitcoin/amoveo

A security flaw was found in this software in this repository that is difficult to fix. I am planning on starting over with a new github project.
The flaw is that if the coins are distributed too thinly amount too many people, that it becomes cheap to bribe the validators to keep a fork alive.

lightning consensus is a bad idea. combining the consensus mechanism with the channel mechanism is a bad idea.
It does increase the amount of voice we get in making consensus decisions, but there are other more efficient ways to achieve this goal.

[non-technical explanation](docs/goal.md)

[innovations in Flying Fox](docs/innovations.md)

[glossary to define words used in cryptoeconomics](docs/glossary.md)

[explains how chanels work. Useful for other projects that want to clone this channel mechanism](docs/channels.md)

[this explains 3 types of attacks that can be done to proof of stake blockchkains, and compares Flying Fox to other PoS mechanisms](docs/failure_modes.md)

[this calculates the portion of money that needs to be colluding with the attacks for the attack to be successful](docs/security.py)

[this folder is for calculating how stable coins will be made from synthetic assets](docs/stablecoin/)

---Code of conduct, development guidelines: Submit pull requests. If I like your code, and it passes tests, I will accept it. Feel free to ask me questions or make github issues.

---Installing

[for ubuntu](compile.md)

