touch temp.db
rm *.db

if [ -d "backup" ]
then
    touch backup/temp.db
    rm backup/*.db
else
    mkdir backup
fi

if [ -e "keys_backup" ]
then
    cp keys_backup keys.db
fi
