#!/usr/bin/env bash

test 0 -eq $EUID || { echo -e "\n\n -- Please run this script using sudo.\n\n -- Example: sudo bash $0\n\n"; exit 0; }

sdb_path=$1
sqlite_file=$2
sdb_backup_gen=$3

sdb_parent_path=$(dirname $sdb_path)
test -d $sdb_parent_path || mkdir -p $sdb_parent_path
setup_logfile="$sdb_parent_path/setup.log"
echo "[start logging]" >> $setup_logfile 
echo "Arg Count=$#" >> $setup_logfile

echo "$1" >> $setup_logfile
echo "$2" >> $setup_logfile
echo "$3" >> $setup_logfile

# 2) Link to shiny server
test -L /srv/shiny-server || ln -s /usr/lib/R/site-library/sampleDB/sampleDB /srv/shiny-server

# 3) Creates / grants access to files + folders
for folder in "backups" "upload_files" "move_files"; do
	test -d "$sdb_parent_path/$folder" || {
		echo "mkdir -p $sdb_parent_path/$folder" >> $setup_logfile;
		mkdir -p "$sdb_parent_path/$folder";
	}
done

test -f $sdb_path || {
	echo "cp $sqlite_file $sdb_path" >> $setup_logfile
	cp $sqlite_file $sdb_path
}

# order matters: grant access last
chmod -R 777 $sdb_parent_path

# 4) Copy backup generator file
test -f "/bin/$(basename $sdb_backup_gen)" || {
	echo "cp $sdb_backup_gen /bin/" >> $setup_logfile;
	cp $sdb_backup_gen "/bin/";
} 

echo "[end logging]" >> $setup_logfile
exit 0