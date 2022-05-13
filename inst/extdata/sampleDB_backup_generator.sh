#!/bin/bash


# Run this  script to backup the sampleDB database.
# Script runs everytime the app is fired up, and every day at midnight
DATE=$(date +%H:%M:%S_%d-%m-%Y)
BACKUP_DIR="/var/lib/sampleDB/backups"

# get path to current DB
CURRENT_DB=/var/lib/sampleDB/sampledb_database.sqlite

# save most recent backup to a temp file
TEMP_FILE_PATH=$(mktemp)
MOST_RECENT_BACKUP=$(ls -Artd /var/lib/sampleDB/backups/* | tail -n 1 )
gunzip<$MOST_RECENT_BACKUP> $TEMP_FILE_PATH

# show the files being compared
echo "Active database is at /var/lib/sampleDB/sampledb_database.sqlite"
echo "Most recent backup is at ${MOST_RECENT_BACKUP}"

# see if differences between the current db and the most recent backup exist
SQL_DIFF_QUANTITY=$(sqldiff $CURRENT_DB $TEMP_FILE_PATH | wc -l)

# if there are differences then backup the database
if [ $SQL_DIFF_QUANTITY -ne 0 ];then
	echo "Backing up the database."
	sqldiff $CURRENT_DB $TEMP_FILE_PATH

	sqlite3 /var/lib/sampleDB/sampledb_database.sqlite ".backup ${BACKUP_DIR}/sampledb-${DATE}.sqlite"
	gzip ${BACKUP_DIR}/sampledb-${DATE}.sqlite
else
	printf "No changes have been made since last backing up the database.\n"
fi

#delete files older than 10 days
#find $BACKUP_DIR/* -mtime +10 -exec rm {} \;
