# SampleDB

## Overview

SampleDB is a Management System for Wet Lab Samples. It can be used to track a sample's `storage address` and `metadata`.
SampleDB is installed as an R package and is accessible via an RShiny web app.

## Running SampleDB

After minimal installation and setup SampleDB can be accessed at the url:
`http://<ip address>:3838/sampleDB/`. This 


1. Uniquely Barcoded Samples
2. Linux Server
3. R >= 4.0
4. [Rstudio Server](https://www.rstudio.com/products/rstudio/download-server/) or ![Rstudio Workbench](https://www.rstudio.com/products/workbench/)
5. [Shiny Server](https://www.rstudio.com/products/shiny/download-server/)
6. [sqlite3](https://www.sqlite.org/position.html)

## Installation

Please take a second to review the below sections before installing the application.

### Hosted versus local application instances

The sampleDB shiny application can be hosted by a shiny server or can be run locally. If you would like to link the
application to the server, you must install the application into any of the site library pathways. Otherwise, it is okay
to install the package locally (or at the site level) and run the shiny application using `sampleDB::Run_SampleDB()`. 

### Data availability

Data access is another important consideration, and should affect your installation choice. If you would like all users to access the same database, you must install using a site library pathway. Local installs will install data files to your operating system's default location for user data, and therefore will prevent other users from accessing that data. System
installs will place the database into the default location for shared application data and will be accessble to anyone who
uses the application.

### Command to setup external environment

#### Site Install

To install sampleDB at the site level, you can run the command below using an R process with elevated privileges:

```R
devtools::install_github(
    "https://github.com/EPPIcenter/sampleDB-rpackage", 
    ref = "v1.2.0",
    lib = .Library[1]
)
```

To run an elevated R process, you can run `sudo R` in your terminal. You can also launch an elevated rstudio instance by opening a terminal and running `sudo rstudio`. 

#### Local Install

For a local install, the below command is sufficient within a regular RStudio or terminal launch:

```R
devtools::install_github(
    "https://github.com/EPPIcenter/sampleDB-rpackage", 
    ref = "v1.2.0"
)
```

## Set Up

To set up sampleDB, run the command below. 

```R
library(sampleDB)
SampleDB_Setup()
```

As previously mentioned, there are a variety of ways to install the application. Below is example output from `SampleDB_Setup()` from site install that does **NOT** have shiny server installed:

```bash
── Deploying sampleDB Environment ──────────────────────────────────────────────
✔ Database location set [/usr/local/share/sampleDB/sampledb_database.sqlite]
✔ Database installed [/usr/local/share/sampleDB/sampledb_database.sqlite]
✔ Subdirectory installed [/usr/local/share/sampleDB/backups]
✔ Subdirectory installed [/usr/local/share/sampleDB/upload_files]
✔ Subdirectory installed [/usr/local/share/sampleDB/move_files]
── Deploying sampleDB Shiny Application ────────────────────────────────────────
✖ Shiny server is not installed.
```

The `✔` indicates successful installation of a directory or file, whereas `✖` indicates a process did not run sucessfully. You will only see `✖` if there is an issue linking the application to shiny server.

Both setup processes will always run: `Deploying sampleDB Environment` runs first, followed by `Deploying sampleDB Shiny Application`. You will always see both processes during setup - there is currently no way to turn off the second process. 

## Database Backups

The database will backup each time a session begins, *unless* the database has not changed since the last backup. Only the 
most recent 10 backups will be kept. The location of the backup folder can be found running `SampleDB_Setup()` again (this function does not overwrite), and it is recommended that you keep additional copies of your backups. 

One of the easiest ways to create a backup is through the `Backup_SampleDB()` function provided in the package. This function by default creates a backup in the default backup folder, although you may pass a filepath to `backup_dest` (e.g. /path/to/file.backup) to overwrite this default behavior.

## FAQ 

### MacOS Permission Issues
If you are working on a Mac and run into permission issues, you may need to run SampleDB with elevated permissions. 

To run Rstudio as an administrator: 
- Go to Applications, then right click on RStudio and Select "Show Package Contents"
- Navigate from Contents > MacOS, in this directory you will see the `RStudio.exec`
- In Terminal, enter `sudo` and then the path to the `RStudio.exec` (alternatively you can drag and drop `RStudio.exec` into Terminal)

Example
```
sudo /Applications/RStudio.app/Contents/MacOS/RStudio
```
- Press enter
- Now RStudio will launch with admin access
