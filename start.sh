#if you want to use a different port, then start like this:
# sh start 3666

<<<<<<< HEAD
#sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
./rebar get-deps
#rm -r ebin #this deletes the existing compiled code.
./rebar compile #this line recompiles the erlang modules.
=======
sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
>>>>>>> origin/master
echo "GO TO THIS WEBSITE -------> http://localhost:3011/main.html"
#sleep 1
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(flying_fox), serve:pw($1)"

