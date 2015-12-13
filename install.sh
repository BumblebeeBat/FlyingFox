#first install rebar package manager
if [ -e "rebar" ]
then
    echo "rebar already installed"
else
    curl https://raw.githubusercontent.com/wiki/rebar/rebar/rebar -o rebar
    chmod u+x rebar
fi

#add erlang run time system to path
PATH=$PATH:./rel/flying_fox/erts-6.4/bin

#use rebar to install other dependencies, explained in rebar.config
./rebar get
./rebar compile

sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
