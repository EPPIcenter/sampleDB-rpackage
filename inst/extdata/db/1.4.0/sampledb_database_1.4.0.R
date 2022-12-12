setwd("~/Desktop")

library(dplyr)
library(sampleDB)
library(stringr)

database <- file.path("~", "Desktop", "sampledb_database.sqlite")
next_database_schema <- "/home/bpalmer/Documents/GitHub/sampleDB-rpackage/inst/extdata/db/1.4.0/sampledb_database_1.4.0.sql"

con <- DBI::dbConnect(RSQLite::SQLite(), database)

pkgname <- 'sampleDB'
current_db_version <- "1.0.0"
next_db_version <- "1.4.0"

target <- tempfile()
system2("sqlite3", args = c(target, "<", next_database_schema))
con2 <- DBI::dbConnect(RSQLite::SQLite(), target)
DBI::dbBegin(con2)


tryCatch({

  for (tab in DBI::dbListTables(con)) {

    print(tab)
    if ("matrix_tube" %in% tab) {
      x <- tbl(con, 'matrix_tube') %>%
        filter(str_length(well_position) == 2) %>%
        collect() %>%
        mutate(xsplit = strsplit(well_position, split=""))

      for (i in 1:nrow(x)) {
        if (x[i, ]$well_position == "NA" || grepl("[A-H]0", x[i, ]$well_position)) {
          print(paste('fix up:', x[i, ]$well_position))
          x[i, ]$well_position <- NA
          next
        }

        z<-unlist(x$xsplit[i])

        x[i, ]$well_position <- paste(c(z[1], "0", z[2]), collapse = "")
      }

      fixed_well_positions <- x %>% select(-c("xsplit")) %>% rename(position = well_position, manifest_id = plate_id)

      if (DBI::dbExistsTable(con2, 'micronix_tube')) {

        x <- tbl(con, "matrix_tube") %>% collect()
        new_rows <- !((x %>% pull(id)) %in% (fixed_well_positions %>% pull(id)))

        df.payload <- rbind(fixed_well_positions, x[new_rows, ] %>%
                              rename(position = well_position,  manifest_id = plate_id))

        df.payload <- df.payload %>%
          arrange(id)

        # set positions to NA - the positions were just copiies of their barcodes
        df.payload[df.payload$manifest_id == 254, ]$position <- NA


        DBI::dbAppendTable(con2, "micronix_tube", df.payload)
      }
    }

    else if ("box" %in% tab) {
      if (DBI::dbExistsTable(con2, 'cryovial_box')) {
        x <- tbl(con, 'box') %>%
          rename(
            name = box_name,

          ) %>%
          collect()

        DBI::dbAppendTable(con2, "cryovial_box", x)
      }
    }

    else if ("tube" %in% tab) {
      if (DBI::dbExistsTable(con2, 'cryovial_tube')) {
        x <- tbl(con, 'tube') %>%
          rename(
            barcode = label,
            manifest_id = box_id,
            position = box_position
          ) %>%
          collect()

        DBI::dbAppendTable(con2, "cryovial_tube", x)
      }
    }

    else if ("matrix_plate" %in% tab) {
      if (DBI::dbExistsTable(con2, 'micronix_plate')) {
        x <- tbl(con, 'matrix_plate') %>%
          rename(
            barcode = plate_barcode,
            name = plate_name
          ) %>%
          collect()

        DBI::dbAppendTable(con2, "micronix_plate", x)
      }
    }

    else if("specimen_type" %in% tab) {
      if (DBI::dbExistsTable(con2, 'specimen_type')) {
        x <- tbl(con, 'specimen_type') %>%
          rename(
            name = label
          ) %>%
          collect()

        DBI::dbAppendTable(con2, "specimen_type", x)
      }
    }

    else if("storage_container" %in% tab) {
      if (DBI::dbExistsTable(con2, 'storage_container')) {
        x <- tbl(con, 'storage_container') %>%
          collect()

        DBI::dbAppendTable(con2, "storage_container", x)
      }
    }

    else if ("study_subject" %in% tab) {
      if (DBI::dbExistsTable(con2, 'study_subject')) {
        x <- tbl(con, 'study_subject') %>%
          rename(
            name = subject
          ) %>%
          collect()

        DBI::dbAppendTable(con2, "study_subject", x)
      }
    }

    else if (tab %in% c("location", "specimen", "study")) {
      if (DBI::dbExistsTable(con, tab)) {
        x <- tbl(con, tab) %>%
          collect()

        DBI::dbAppendTable(con2, tab, x)
      }
    }

    else if ("version" %in% tab) {
      if (DBI::dbExistsTable(con, 'version')) {
        x <- tbl(con, 'version') %>%
          mutate(name = next_db_version) %>%
          collect()

        DBI::dbAppendTable(con2, "version", x)
      }
    }
  }

  DBI::dbCommit(con2)

  if (file.exists(destination))
    file.remove(destination)

  file.copy(target, destination)

},
warning = function(w) {
  DBI::dbRollback(con2)
  message(w)
},
error = function(e) {
  DBI::dbRollback(con2)
  message(e)
},
finally = {
  DBI::dbDisconnect(con)
  DBI::dbDisconnect(con2)
})



