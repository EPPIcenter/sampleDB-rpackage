#!/bin/bash


# Run this  script to backup the sampleDB database.
# Script runs everytime the app is fired up, and every day at midnight
DATE=$(date +%H:%M:%S_%d-%m-%Y)
BACKUP_DIR=/srv/sampleDB/backups

# get path to current DB
CURRENT_DB=/srv/sampleDB/sampledb_database.sqlite

# save most recent backup to a temp file
TEMP_RECENT_BACKUP=$(mktemp)
TEMP_CURRENT_BACKUP=$(mktemp)

# backup the current database to a temporary location
sqlite3 $CURRENT_DB ".backup $TEMP_CURRENT_BACKUP"

# if we have a backup, compare against the most recent
if [ 0 -eq $(ls $BACKUP_DIR | wc -l) ]; then
  echo "Creating the first backup!"
	cat $TEMP_CURRENT_BACKUP | gzip > ${BACKUP_DIR}/sampledb-${DATE}.sqlite.gz
else
  MOST_RECENT_BACKUP=$(ls -Artd $BACKUP_DIR/* | tail -n 1 )
  gunzip<$MOST_RECENT_BACKUP> $TEMP_RECENT_BACKUP

  # show the files being compared
  echo "Active database is at /srv/sampleDB/sampledb_database.sqlite"
  echo "Most recent backup is at ${MOST_RECENT_BACKUP}"

  test 2 -eq $(md5sum $TEMP_RECENT_BACKUP $TEMP_CURRENT_BACKUP | cut -f 1 -d ' ' | sort | uniq | wc -l) && {
    echo "Backing up the database.";
    cat $TEMP_CURRENT_BACKUP | gzip > ${BACKUP_DIR}/sampledb-${DATE}.sqlite.gz;
  } || {
    echo "No changes have been made since last backing up the database.";
  }
fi

#delete files older than 10 days
#find $BACKUP_DIR/* -mtime +10 -exec rm {} \;
