# sampleDB

## Overview

SampleDB is a Management System for Wet Lab Samples. It can be used to track a sample's `storage address` and `metadata`.
SampleDB is installed as an R package and is accessible via an RShiny web app.

## Running SampleDB

After minimal installation and setup SampleDB can be accessed at the url:
`http://<ip address>:3838/sampleDB/`


1. Uniquely Barcoded Samples
2. Linux Server
3. R >= 4.0
4. ![Rstudio Server](https://www.rstudio.com/products/rstudio/download-server/) or ![Rstudio Workbench](https://www.rstudio.com/products/workbench/)
5. ![Shiny Server](https://www.rstudio.com/products/shiny/download-server/)
6. ![sqlite3]()

## Installation

The sampleDB shiny application can be hosted by shiny server or can be run locally. If you would like to link the
application to the server, you must install the application into any of the site library pathways. Otherwise, it is okay
to install the package locally and run the application using `sampleDB::Run_SampleDB()`.

Data access is another important consideration, and should affect your installation choice. If you would like all users to access the same database, you must install using a site library pathway. Local installs will install data files to your operating system's default location for user data, and therefore will prevent other users from accessing that data. System
installs will place the database into the default location for shared application data and will be accessble to anyone who
uses the application.

To install sampleDB at the site level, you can run the command below using an R process with elevated privileges:

```R
devtools::install_github(
    "https://github.com/EPPIcenter/sampleDB-rpackage", 
    ref = "v1.1.0",
    lib = .Library[1])
)
```

## Set Up

To set up SampleDB run the command below. You will be prompted to carry out the setup.

```R
library(sampleDB)
SampleDB_Setup()
```

## Database Schema
![SampleDB v2](https://user-images.githubusercontent.com/95319271/161106124-afd9ddd6-bdcd-4914-b903-a2d66b454991.jpg)

## Backups

The database will be backed up each time a session begins, unless the database has not changed since the last backup. Only the 
most recent 10 backups will be kept. The location of the backup folder can be found running `SampleDB_Setup()` again (this function does not overwrite). It is recommended that you keep additional copies of your backups. 

There are a number of way to backup the database manually or on a schedule. One of the easiest ways is to use the `Backup_SampleDB()` function provided in the package. This function by default will backup the database to the default backup folder. If you would like to back the database up somewhere else, you can pass a filepath to `backup_dest` (e.g. /path/to/file.backup).

