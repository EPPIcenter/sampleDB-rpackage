
# Running SampleDB
To spin up the app run the command below in the console of an Aragorn instance of R.
```
sampleDB::Run_SampleDB()
```

# The Database
The database lives on Aragorn (`128.218.162.121`) at `/databases/sampledb/v0.0.2/sampledb_database.sqlite`.
The packages uses the R environment variable `SDB_PATH` to connect to the database.
To set this variable edit the Renviron file (which typically exists at: `/etc/R/Renviron`). Use `Sys.getenv("SDB_PATH")` to check that edits have taken affect.

## Database Schema
![db](https://user-images.githubusercontent.com/95319271/151049473-54f411e9-0fa2-4d3b-be33-32a0d8521a08.png)

# Installation
For **Admins** to install sampleDB on Aragorn use:
```
withr::with_libpaths(new = "/usr/lib/R/site-library/", 
		     devtools::install_github("https://github.com/EPPIcenter/sampleDB-rpackage", 
		     			      ref = "master", 
					      auth_token = "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"))
```

System-wide Aragorn R packages are installed at `/usr/lib/R/site-library/`.

# Migration from SDB v1 -> SDB v2
In order to migrate from SDB v1 to SDB v2 use the script in `helpers/migrate_old_sdb.R`, following each step in the script. 
To then update the freezer addresses for each matrix plate use the script `helpers/assign_freezer_addresses.R`

# Backups
- Backups are located at `/databases/sampleDB_backups/`
- Cronjob to create backup is located at `/bin/sampleDB_backup_generator.sh`. 
- Currently backups are created everyday at 16:15:00
