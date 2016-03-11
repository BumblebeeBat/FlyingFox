#sh start.sh 3010 &
#sh start.sh 3020 &

#pubkey
curl -i -d '["pubkey"]' http://localhost:3011


curl -i -d '["key_unlock", "YWJj"]' http://localhost:3021
curl -i -d '["key_unlock", "YWJj"]' http://localhost:3031

curl -i -d '["keys_id_update", 2]' http://localhost:3021
curl -i -d '["keys_id_update", 1]' http://localhost:3031
curl -i -d '["keys_id_update", 0]' http://localhost:3011

