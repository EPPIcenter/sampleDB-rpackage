#!/bin/bash
DATE=$(date +%H:%M:%S_%d-%m-%Y)
BACKUP_DIR="/var/lib/sampleDB/backups"

# take each website’s backup in separate name, use below format #
gzip < /var/lib/sampleDB/sampledb_database.sqlite > $BACKUP_DIR/sampledb-$DATE.gz

# Delete files older than 10 days #
# find $BACKUP_DIR/* -mtime +10 -exec rm {} \;
