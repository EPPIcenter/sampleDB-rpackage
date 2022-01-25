
# Installation
For **Admins** to install sampleDB on Aragorn use:
```
withr::with_libpaths(new = "/usr/lib/R/site-library/", 
		     devtools::install_github("https://github.com/EPPIcenter/sampleDB-rpackage", 
		     			      ref = "master", 
					      auth_token = "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"))
```

System-wide Aragorn R packages are installed at `/usr/lib/R/site-library/`.

# The Database
The database lives at `/databases/sampledb_database.sqlite`.
![db](https://user-images.githubusercontent.com/95319271/151049292-96aae7e9-9f4a-489b-a5d6-332b1ad66518.png)

# Backups
- Backups are located at `/databases/sampleDB_backups/`
- Cronjob to create backup is located at `/bin/sampleDB_backup_generator.sh`. 
- Currently backups are created everyday at 16:15:00
