#first install rebar package manager
if [ -e "rebar" ]
then
    echo "rebar already installed"
else
    curl https://raw.githubusercontent.com/wiki/rebar/rebar/rebar -o rebar
    chmod u+x rebar
fi
#use rebar to install other dependencies, explained in rebar.config
./rebar get
./rebar compile

