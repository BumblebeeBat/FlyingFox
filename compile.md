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