#the release is in the rel directory. you can start it with the commandf: 
# `make run`mkdir rel

cd rel
wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
make -f erlang.mk bootstrap bootstrap-rel 
cp -r ../src .
cp -r ../deps .
make


