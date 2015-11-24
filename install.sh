#first install rebar package manager
if [ -e "rebar" ]
then
    echo "rebar already installed"
else
    git clone git://github.com/rebar/rebar.git rebar_source
    cd rebar_source/
    ./bootstrap
    cp rebar ..
    cd ..
fi
#use rebar to install other dependencies, explained in rebar.config
./rebar get
./rebar compile
erlc src/networking/port.erl
mkdir ebin
mv port.beam ebin
