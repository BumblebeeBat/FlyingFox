#first install rebar package manager
if [ -e "rebar" ]
then
    echo "rebar already installed"
else
    wget https://github.com/rebar/rebar/wiki/rebar && chmod u+x rebar
fi
#use rebar to install other dependencies, explained in rebar.config
./rebar get
./rebar compile

