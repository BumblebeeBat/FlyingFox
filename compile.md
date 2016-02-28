First, make sure you have erlang installed. Version 18 is prefered, but older versions will probably work. Here is one way to download it: http://www.erlang.org/download.html , here are erlang install instructions: http://www.erlang.org/doc/installation_guide/INSTALL.html

For ubuntu, I needed to install dependencies:

```
sudo apt-get install libncurses5-dev
sudo apt-get install libssl-dev
sudo apt-get install unixodbc-dev
sudo apt-get install g++ #only sometimes need this.
```
Next, download Flying Fox.

```
#for linux
wget https://github.com/BumblebeeBat/FlyingFox/archive/development.zip
unzip development.zip
#for mac, use curl.
```
Now you can go into the directory, and compile flying fox.

```
cd FlyingFox-development/
sh install.sh
```
Start your node with this script:

```
sh start.sh
```
Then open this URL in your browser: http://localhost:3011/login.html
