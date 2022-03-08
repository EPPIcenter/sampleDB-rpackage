library(magrittr)
library(dplyr)
library(sampleDB)
args <- commandArgs(trailingOnly = TRUE)

# in order to migrate data from SDB v1 to SDB v2, 
# * each v1 table must be reformatted to match the format of of it's corresponding v2 table
# * then the reformatted tables must be written out as csvs and saved locally so that migration via db browser can be performed
# * once reformatted files are saved locally they can be imported, table by table, in to a blank sdb v2 database - the order that tables are imported matters, foregin keys need to be in the db before tables can be imported
# * at this point close the db browser connection to the database and move the table back onto aragorn

# ------- functions
print_db <- function(new_sdb){
  for(tbl in ListTables(database = new_sdb)){
    tmp_tbl <- CheckTable(database = new_sdb, tbl)
    if(nrow(tmp_tbl) > 0){
      print(tbl)
      print(tmp_tbl)
    }
  } 
}

# ------- read in data

# old_sdb <- args[1]
# new_sdb <- args[2]

old_sdb <- "/databases/sampledb/scratch/07-Mar-2022.v0.01.sqlite"
new_sdb <- "/databases/sampledb/scratch/sampledb_database.sqlite"

print(paste("old db is at ->", old_sdb))
print(paste("new db is at ->", new_sdb))

# ------ reformat references
# location
old.location <- CheckTable(database = old_sdb, "location")

new.location <- old.location %>% 
  mutate(created = created %>% gsub("\\..*","",.),
         last_updated = last_updated %>% gsub("\\..*","",.),
         location_type = "dummy",
         level_I = "dummy",
         level_II = "dummy",
         level_III = as.character(NA)) %>%
  rename(location_name = description) %>% 
  relocate(id, .before = location_name)

write.table(file = "~/location.csv", new.location, sep = ",", row.names = FALSE, col.names = FALSE)

# studies
old.study <- sampleDB::CheckTable(database = old_sdb, "study")

new.study <- old.study %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = title) %>%
  select(-hidden)

write.table(file = "~/study.csv", new.study, sep = ",", row.names = FALSE, col.names = FALSE)

# specimen types
old.specimen_type <- sampleDB::CheckTable(database = old_sdb, "specimen_type")

new.specimen_type <- old.specimen_type %>% 
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = label)

write.table(file = "~/specimen_type.csv", new.specimen_type, sep = ",", row.names = FALSE, col.names = FALSE)

# ----- reformat internal data
# storage_container
old.storage_container <- sampleDB::CheckTable(database = old_sdb, "storage_container")

new.storage_container <- old.storage_container %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.),
         type = "micronix") %>%
  relocate(id, .before = type) %>%
  relocate(specimen_id, .before = type) %>%
  select(-c(comments))

write.table(file = "~/storage_container.csv", new.storage_container, sep = ",", row.names = FALSE, col.names = FALSE)

# specimens
old.specimen <- sampleDB::CheckTable(database = old_sdb, "specimen")

new.specimen <- old.specimen %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = study_subject_id)

write.table(file = "~/specimen.csv", new.specimen, sep = ",", row.names = FALSE, col.names = FALSE)

# subjects
old.study_subject <- sampleDB::CheckTable(database = old_sdb, "study_subject")

new.study_subject <- old.study_subject %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = uid) %>%
  rename(subject = uid) %>%
  relocate(study_id, .before = subject)

write.table(file = "~/study_subject.csv", new.study_subject, sep = ",", row.names = FALSE, col.names = FALSE)

# ----- external
# matrix_plate
old.matrix_plate <- sampleDB::CheckTable(database = old_sdb, "matrix_plate")

new.matrix_plate <- old.matrix_plate %>% 
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  rename(plate_name = uid) %>%
  select(-c(hidden)) %>%
  relocate(location_id, .before = plate_name) %>%
  relocate(id, .before = location_id)

write.table(file = "~/matrix_plate.csv", new.matrix_plate, sep = ",", row.names = FALSE, col.names = FALSE)

# matrix_tube
old.matrix_tube <- sampleDB::CheckTable(database = old_sdb, "matrix_tube")

new.matrix_tube <- old.matrix_tube

write.table(file = "~/matrix_tube.csv", new.matrix_tube, sep = ",", row.names = FALSE, col.names = FALSE)

# ------ user messages
print(paste0("migrated v0.0.1 ", "(", old_sdb, ")", " -> ", "v0.0.2 ", "(", new_sdb, ")"))
