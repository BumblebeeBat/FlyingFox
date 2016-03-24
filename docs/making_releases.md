#first install flying fox to compile the pieces:
# sh install.sh

#then make a directory for the new release, and move into it
# mkdir rel
# cd rel

#now download the release tool

wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

#more stuff to do
make -f erlang.mk bootstrap bootstrap-rel 

#rename stuff to flying_fox

cp -r ../src .
cp -r ../deps .

make

#then you can run it with: make run