#if you want to use a different port, then start like this:
# sh start 3666

#sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
./rebar get-deps
#rm -r ebin #this deletes the existing compiled code.
./rebar compile #this line recompiles the erlang modules.
echo "GO TO THIS WEBSITE -------> http://localhost:3011/main.html"
#sleep 1
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(flying_fox), serve:pw($1)"

