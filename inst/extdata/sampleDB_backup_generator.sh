#!/bin/bash


# Run this  script to backup the sampleDB database.
# Script runs everytime the app is fired up, and every day at midnight
DATE=$(date +%H:%M:%S_%d-%m-%Y)
BACKUP_DIR="/var/lib/sampleDB/backups"

# get mdh sum of database
CURRENT_MD5=$(md5sum /var/lib/sampleDB/sampledb_database.sqlite | awk '{print $1}')

#get mdh sum of most recently created backup file
MOST_RECENT_MD5=$(gunzip<$(ls -Artd /var/lib/sampleDB/backups/* | tail -n 1) | md5sum | awk '{print $1}')

#if md5sum of database does not equal the md5sum of the most recent backup then generate a new backup
printf "$CURRENT_MD5 $MOST_RECENT_MD5 \n"
if [ "$CURRENT_MD5" != "$MOST_RECENT_MD5" ];then
	echo "Backing up the database."
	gzip < /var/lib/sampleDB/sampledb_database.sqlite > $BACKUP_DIR/sampledb-$DATE.gz
else
	echo "No changes have been made since last backing up the database."
fi

#delete files older than 10 days
#find $BACKUP_DIR/* -mtime +10 -exec rm {} \;
