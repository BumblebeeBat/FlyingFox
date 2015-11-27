Flying Fox
==========

[![Join the chat at https://gitter.im/BumblebeeBat/FlyingFox](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BumblebeeBat/FlyingFox?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT

Flying Fox is my attempt to achieve the goals of the truthcoin project: http://www.truthcoin.info/ I respect Paul Sztorc, especially in his ability to communicate truthcoin, which he invented. Him and I are using very different strategies to try and build the same thing.

Sztorc maintains a forum where the different teams trying to build truthcoin can share ideas: http://forum.hive-mind.info/

Facts about Flying Fox:

*It is not based on bitcoin. It is a new blockchain with a new consensus mechanism optimized for channels.

*Does not used bitcoin, is a new cryptocurrency.

*Finite memory requirement of 2 gigabytes.

*Written in the erlang programming language

*Betting is inside of the lightning network.

*Judgement on the outcome of bets occurs off-chain, in the lightning network.

Code of conduct, development guidelines: Submit pull requests. If I like, and it passes tests, I will accept. Feel free to ask me questions or make github issues.

Install instructions:

Eventually Flying Fox will be a simple executable you click on. For now it is a bit more complex.

First, make sure you have erlang installed. Version 18 is prefered, but older versions will probably work. Here is one way to download it: http://www.erlang.org/download.html , here are erlang install instructions: http://www.erlang.org/doc/installation_guide/INSTALL.html

For ubuntu, I needed to install dependencies:

```
sudo apt-get install libncurses5-dev
sudo apt-get install libssl-dev
sudo apt-get install unixodbc-dev
```

It needed to be configured, make-ed, 

```
./configure
make
sudo make install
```

Next, download Flying Fox.

```
wget https://github.com/BumblebeeBat/FlyingFox/archive/development.zip
unzip development.zip
```

Now you can go into the directory, and install Flying Fox.

```
cd FlyingFox-development/
sh install.sh
```

Start your node with this script:

```
sh start.sh
```

Then open this URL in your browser: http://localhost:3011/main.html