rebar compile
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(flying_fox)"
