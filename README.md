
# Running SampleDB
After installation and database file configuration, to spin up the SampleDB app run the command below in an instance of R.
```
library(sampleDB)
Run_SampleDB()
```
# Requirements

All that is needed at this time is an installation of R >= 4.0

# Installation
To install sampleDB use:
```
withr::with_libpaths(new = "/usr/lib/R/site-library/", 
                     devtools::install_github("https://github.com/EPPIcenter/sampleDB-rpackage", 
                                              ref = "master", 
                                              auth_token = "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"))
```

# The Database
The database file that SampleDB reads and writes from is a SQLite3 file, a template for this database is provided at `databases/sampledb_template.sqlite` or can be downloaded from ![here](https://drive.google.com/file/d/1umwodPMPR0kZdsrlxTJQa-O0ylQq4tUS/view?usp=sharing).
Copy this file to a new location on your filesystem and make it readable and writable for all users.
Once the file is placed in a new location the path to the file needs to be saved in the `Renviron.site` file as `SDB_PATH`.
For example...
```
SDB_PATH="/path/to/the/database"
```

The default location of `Renviron.site` is 'R_HOME/etc/Renviron.site'
`Renviron.site` is the first file that gets read when an instance of R is spun up and variables in this file can be accessed by all R users.

To check that `SDB_PATH` matches the path to your database file run.
```
Sys.getenv("SDB_PATH")
```

## Database Schema
![SampleDB v2](https://user-images.githubusercontent.com/95319271/159344494-62fb6d59-66b6-4a9a-b4ae-decd74fc9739.svg)

# Backups
- Backups are located at `/databases/sampleDB_backups/`
- Cronjob to create backup is located at `/bin/sampleDB_backup_generator.sh`. 
- Backups are whenever someone issues the `Run_SampleDB()` command
- Backups are also generated every day at midnight and noon.

# How to approach SampleDB? What is sampleDB?

# Migration from SDB v1 -> SDB v2
In order to migrate from SDB v1 to SDB v2 use the script in `helpers/migrate_old_sdb.R`, following each step in the script. 
To then update the freezer addresses for each matrix plate use the script `helpers/assign_freezer_addresses.R`
