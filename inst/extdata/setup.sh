#!/usr/bin/env bash

test 0 -eq $EUID || { echo -e "\n\n -- Please run this script using sudo.\n\n -- Example: sudo bash $0\n\n"; exit 0; }

sdb_path=$1
sqlite_file=$2
sdb_environ_file=$3

sdb_parent_path=$(dirname $sdb_path)
test -d $sdb_parent_path || mkdir -p $sdb_parent_path
setup_logfile="$sdb_parent_path/setup.log"
echo "[start logging]" >> $setup_logfile 
echo "Arg Count=$#" >> $setup_logfile

echo "$1" >> $setup_logfile
echo "$2" >> $setup_logfile
echo "$3" >> $setup_logfile

# 1) SDB file
test 0 -eq $(grep 'SDB_PATH' $sdb_environ_file | wc -l) && {
	echo "SDB_PATH=$sdb_path" >> $sdb_environ_file;
} || {
	sdb_path_escaped=$(echo $sdb_path | sed -e 's/\//\\\//g');
	sed -i "/SDB_PATH/s/=.*$/=$sdb_path_escaped/" $sdb_environ_file;
}

# 2) Link to shiny server
test -L /srv/shiny-server || ln -s /usr/lib64/R/site-library/sampleDB/sampleDB /srv/shiny-server

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

echo "[end logging]" >> $setup_logfile
exit 0