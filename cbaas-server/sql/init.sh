#! /bin/bash

PGUSER=cbaas
PASS=$1
ROOTUSER=$2  #On Ubuntu, use postgres. On osx, the user with postgres admin access
DB=cbaas


if [ $# -ne 2 ]; then
   echo "Please call setup.sh with database password argument, and root user only"; exit;
fi

read -p "About to reset the cbaas database. Sure? (y/n)" yn
case $yn in
    [Nn]* ) echo "Ok, let's not yet. Did nothing."; exit;;
    [Yy]* )

        sudo -u $ROOTUSER dropdb $DB;
        sudo -u $ROOTUSER dropuser $PGUSER;
        sudo -u $ROOTUSER createuser $PGUSER;
        sudo -u $ROOTUSER createdb $DB;
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "ALTER ROLE $PGUSER WITH PASSWORD '$PASS';";
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "CREATE EXTENSION \"uuid-ossp\";";
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "GRANT ALL PRIVILEGES ON DATABASE $DB TO $PGUSER";;

    * ) echo "Didn't catch that. Try again, please answer y or n"
esac
