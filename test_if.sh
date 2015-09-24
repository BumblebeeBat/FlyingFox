if [ -e "accounts.db" ]
then
    echo "works"
fi

if [ -e "counts.db" ]
then
    echo "no works"
else
    echo "double works"
fi
