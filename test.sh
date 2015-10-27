#sh start.sh 3010 &
#sh start.sh 3020 &

curl -i -d '[-6,"test"]' http://localhost:3011

curl -i -d '[-6,"test"]' http://localhost:3021
