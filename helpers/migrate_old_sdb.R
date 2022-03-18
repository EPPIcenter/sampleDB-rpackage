library(magrittr)
library(dplyr)
library(sampleDB)
library(RSQLite)
args <- commandArgs(trailingOnly = TRUE)

# in order to migrate data from SDB v1 to SDB v2, 
# * cp v2 template to /database scratch space
# * run this script which saves v1 tables in v2 format to the scratch /tbl dir
# * use sqlite on the command line to import each table from /tbl to the template in the scratch space
# - .mode csv
# - .import tbl.csv  tbl
# * move this populated database to /database/sampleDB/v0.0.2/sampledb_database.sqlite

# ------- read in data
# old_sdb <- args[1]
old_sdb <- "/databases/sampledb/scratch/16-Mar-2022.v0.0.1.sqlite"

# ------ reformat references
# location
CheckTable(database = old_sdb, "location") %>% 
  mutate(created = created %>% gsub("\\..*","",.),
         last_updated = last_updated %>% gsub("\\..*","",.),
         location_type = "",
         level_I = "",
         level_II = "") %>%
  rename(location_name = description) %>% 
  relocate(id, .before = location_name) %>%
  write.table(file = "/databases/sampledb/scratch/tbls/location.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# studies
sampleDB::CheckTable(database = old_sdb, "study") %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = title) %>%
  select(-hidden) %>% 
  write.table(file = "/databases/sampledb/scratch/tbls/study.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# specimen types
sampleDB::CheckTable(database = old_sdb, "specimen_type")  %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = label) %>% 
  write.table(file = "/databases/sampledb/scratch/tbls/specimen_type.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# ----- reformat internal data
# storage_container
sampleDB::CheckTable(database = old_sdb, "storage_container") %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.),
         type = "micronix") %>%
  relocate(id, .before = type) %>%
  relocate(specimen_id, .before = type) %>%
  select(-c(comments)) %>% 
  write.table(file = "/databases/sampledb/scratch/tbls/storage_container.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# specimens 
#UPDATE specimen SET collection_date = null WHERE collection_date = "NA"
sampleDB::CheckTable(database = old_sdb, "specimen") %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.),
         collection_date = collection_date %>% lubridate::as_date() %>% as.numeric()) %>%
  relocate(id, .before = study_subject_id) %>% 
  write.table(file = "/databases/sampledb/scratch/tbls/specimen.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# subjects
sampleDB::CheckTable(database = old_sdb, "study_subject") %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = uid) %>%
  rename(subject = uid) %>%
  relocate(study_id, .before = subject) %>%
  write.table(file = "/databases/sampledb/scratch/tbls/study_subject.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# ----- external
# matrix_plate
sampleDB::CheckTable(database = old_sdb, "matrix_plate") %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  rename(plate_name = uid) %>%
  select(-c(hidden)) %>%
  relocate(location_id, .before = plate_name) %>%
  relocate(id, .before = location_id) %>% 
  write.table(file = "/databases/sampledb/scratch/tbls/matrix_plate.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# matrix_tube
sampleDB::CheckTable(database = old_sdb, "matrix_tube") %>%
  write.table(file = "/databases/sampledb/scratch/tbls/matrix_tube.csv", ., sep = ",", row.names = FALSE, col.names = FALSE)

# ------ user messages
message("all tables have been reformatted and are saved to /databases/sampledb/scratch/tbls/")
