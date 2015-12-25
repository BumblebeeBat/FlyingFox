./rebar get
rm -r rel
mkdir rel
cp reltool.config rel/reltool.config
cd rel
../rebar create-node nodeid=flying_fox
cd ..
./rebar compile generate
