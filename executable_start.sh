sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.

./rel/flying_fox/bin/flying_fox start
sleep 5
./rel/flying_fox/bin/flying_fox attach

echo "GO TO THIS WEBSITE -------> http://localhost:3011/main.html"
