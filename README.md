
# Overview
SampleDB is a Management System for Wet Lab Samples. It can be used to track a sample's `storage address` and `metadata`.
SampleDB is installed as an R package and is accessible via an RShiny web app.

# Running SampleDB
After minimal installation and setup SampleDB can be accessed at the url:
`http://<ip address>:3838/sampleDB/`

1. Uniquely Barcoded Samples
2. Linux Server
3. R >= 4.0
4. ![Rstudio Server](https://www.rstudio.com/products/rstudio/download-server/) or ![Rstudio Workbench](https://www.rstudio.com/products/workbench/)
5. ![Shiny Server](https://www.rstudio.com/products/shiny/download-server/)

# Installation
To install SampleDB run the command below in an R instance:
```
withr::with_libpaths(new = "/usr/lib/R/site-library/", 
                     devtools::install_github("https://github.com/EPPIcenter/sampleDB-rpackage", 
                                              ref = "master", 
                                              auth_token = "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"))
```

# Set Up
To set up SampleDB run the command below. You will be prompted to carry out the setup.
```
library(sampleDB)
SampleDB_Setup()
```

# Database Schema
![SampleDB v2](https://user-images.githubusercontent.com/95319271/161106124-afd9ddd6-bdcd-4914-b903-a2d66b454991.jpg)

# Backups

SampleDB is backed up every Sunday to the Mines using `/opt/sample_db/sample_db_backup.sh` and a crontab.

<!---# The Database-->
<!---The database file that SampleDB reads and writes from is a SQLite3 file, a template for this database is provided at--> <!---`databases/sampledb_template.sqlite` or can be downloaded from--> <!---![here](https://drive.google.com/file/d/1umwodPMPR0kZdsrlxTJQa-O0ylQq4tUS/view?usp=sharing).-->
<!---Copy this file to a new location on your filesystem and make it readable and writable for all users.-->
<!---Once the file is placed in a new location the path to the file needs to be saved in the `Renviron.site` file as `SDB_PATH`.-->
<!---For example...-->
<!---```-->
<!---SDB_PATH="/path/to/the/database"-->
<!---```-->

<!---The default location of `Renviron.site` is 'R_HOME/etc/Renviron.site'-->
<!---`Renviron.site` is the first file that gets read when an instance of R is spun up and variables in this file can be accessed by all R users.-->

<!---To check that `SDB_PATH` matches the path to your database file run.-->
<!---```-->
<!---sampleDB:::.GetSampleDBPath()-->
<!---```-->

<!---## Database Schema-->
<!---![SampleDB v2](https://user-images.githubusercontent.com/95319271/159344494-62fb6d59-66b6-4a9a-b4ae-decd74fc9739.svg)-->

<!---# Backups-->
<!---- Backups are located at `/databases/sampleDB_backups/`-->
<!---- Cronjob to create backup is located at `/bin/sampleDB_backup_generator.sh`.-->
<!---- Backups are whenever someone issues the `Run_SampleDB()` command-->
<!---- Backups are also generated every day at midnight and noon.-->
