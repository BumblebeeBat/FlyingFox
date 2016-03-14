#Open up 3 terminals. 
#Copy the Flying Fox install directory so you have 3 copies. 
#Set all 3 passwords to "abc". Use the command keys:new("abc"), then copy keys.db to keys_backup.
#Launch one using port 3010, one on 3020, and one on 3030.
#Then run this script from a fourth terminal.

#It lightning spends 4 coins one way, then spends the same 4 back.

curl -i -d '["key_unlock", "YWJj"]' http://localhost:3011
curl -i -d '["key_unlock", "YWJj"]' http://localhost:3021
curl -i -d '["key_unlock", "YWJj"]' http://localhost:3031

curl -i -d '["keys_id_update", 2]' http://localhost:3021
curl -i -d '["keys_id_update", 1]' http://localhost:3031
curl -i -d '["keys_id_update", 0]' http://localhost:3011

curl -i -d '["create_account", "QkdOUW5mREMzWnU1MEh3SUN3QTJyZ1RYc2JGQUVsNWtCM2VqVE9uVFpLb1hEZUM3c3kyR1hMZzNvaVFBQ29wRVV2UVNpRC9Tb0l2T1VnRkJLd0NHQUg0PQ==", 10000000, 0]' http://localhost:3011
curl -i -d '["create_account", "Qk9ZclBvNjlhaVNRV0w1NGZ6UjJBdFJaQ29JSkxkS2U4WGVlVUVqUGswOU5CY2l4Z0RxZEFrVkxSZ1lNbFJoUVk3bkJ6cno5S1BUYlZtZ0VtdDlRZDZRPQ==", 1000000, 0]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021

curl -i -d '["new_channel", [127,0,0,1], 3030, 1000000, 900000, 50]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
curl -i -d '["new_channel", [127,0,0,1], 3030, 500000, 450000, 50]' http://localhost:3021

curl -i -d '["lightning_spend", [127,0,0,1], 3030, 2, 4]' http://localhost:3011
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
sleep 1


curl -i -d '["lightning_spend", [127,0,0,1], 3030, 0, 4]' http://localhost:3021
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
sleep 1

