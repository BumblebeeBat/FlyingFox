PATH=$PATH:./rel/flying_fox/erts-7.1/bin

sh clean.sh

./rel/flying_fox/erts-7.1/bin/erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(flying_fox), serve:pw($1), keys:unlock(\"abc\")"


# ./rel/flying_fox/bin/flying_fox start
# sleep 5
# ./rel/flying_fox/bin/flying_fox attach

echo "GO TO THIS WEBSITE -------> http://localhost:3011/main.html"
