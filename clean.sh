touch temp.db
rm *.db

if [ -d "backup" ]
then
    touch backup/temp.db
    rm backup/*.db
else
    mkdir backup
fi

cp keys_backup keys.db
