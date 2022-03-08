library(magrittr)
library(dplyr)
library(sampleDB)
args <- commandArgs(trailingOnly = TRUE)

# go into scratch space... cp v0.0.2 template and most up to date v0.0.1
# run script "Rscript ~/lib/packages/sampleDB-rpackage/aid/migrate_old_sdb.R 22-Feb-2022.v0.0.1.sqlite sampledb_database.sqlite"
# old_sdb <- "/databases/sampledb/scratch/sample_db.sqlite"
# new_sdb <- "/databases/sampledb/scratch/sampledb_database.sqlite"

# old_sdb <- args[1]
# new_sdb <- args[2]

old_sdb <- "/databases/sampledb/scratch/07-Mar-2022.v0.01.sqlite"
new_sdb <- "/databases/sampledb/scratch/sampledb_database.sqlite"

for(tbl in ListTables(database = new_sdb)){
  tmp_tbl <- CheckTable(database = new_sdb, tbl)
  if(nrow(tmp_tbl) > 0){
    print(tbl)
    print(tmp_tbl)
  }
}

print(paste("old db is at ->", old_sdb))
print(paste("new db is at ->", new_sdb))

# AddTblToDB <- function(table.name, table.obj){
#   con <- RSQLite::dbConnect(RSQLite::SQLite(), new_sdb)
#   RSQLite::dbSendQuery(con, paste0("DROP TABLE ", table.name,";"))
#   RSQLite::dbWriteTable(con, table.name, table.obj)
#   RSQLite::dbDisconnect(con)
#   
#   sampleDB::CheckTable(database = new_sdb, table.name)
# }

########## GET OLD DB INFO
# ----- references
# freezers - c("created", "last_updated", "id", "location_name", "location_type", "level_I", "level_II", "level_III")
old.location <- sampleDB::CheckTable(database = old_sdb, "location")

location <- old.location %>% 
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.),
         location_type = "dummy",
         level_I = "dummy",
         level_II = "dummy",
         level_III = NA) %>%
  rename(location_name = description)

for (i in 1:nrow(location)){
  # print(i)
  info_list <- list(created = location[i,]$"created",
                   last_updated = location[i,]$"last_updated",
                   location_name = location[i,]$"location_name",
                   location_type = location[i,]$"location_type",
                   level_I = location[i,]$"level_I",
                   level_II = location[i,]$"level_II",
                   level_III = location[i,]$"level_III")
  
  # print(info_list)
  sampleDB::AddToTable(database = "/databases/sampledb/scratch/sampledb_database.sqlite",
                       table_name = "location",
                       info_list = info_list)
}

# studies - c("created", "last_updated", "id", "title", "description", "short_code", "is_longitudinal", "lead_person")
old.study <- sampleDB::CheckTable(database = old_sdb, "study")

study <- old.study %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.))

for (i in 1:nrow(study)){

  info_list <- list(created = study[i,]$"created",
                    last_updated = study[i,]$"last_updated",
                    title = study[i,]$"title",
                    description = study[i,]$"description",
                    short_code = study[i,]$"short_code",
                    is_longitudinal = study[i,]$"is_longitudinal",
                    lead_person = study[i,]$"lead_person")
  
  sampleDB::AddToTable(database = "/databases/sampledb/scratch/sampledb_database.sqlite",
                       table_name = "study",
                       info_list = info_list)

}

# specimen types - c("created",  "last_updated", "id",  "label")
old.specimen_type <- sampleDB::CheckTable(database = old_sdb, "specimen_type")

specimen_type <- old.specimen_type %>% 
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.))

for (i in 1:nrow(specimen_type)){
  
  info_list <- list(created = specimen_type[i,]$"created",
                    last_updated = specimen_type[i,]$"last_updated",
                    label = specimen_type[i,]$"label")
  
  sampleDB::AddToTable(database = "/databases/sampledb/scratch/sampledb_database.sqlite",
                       table_name = "specimen_type",
                       info_list = info_list)
}

# ----- internal
# storage_container - c("created",  "last_updated", "id",  "specimen_id", "type",  "exhausted")
old.storage_container <- sampleDB::CheckTable(database = old_sdb, "storage_container")

storage_container <- old.storage_container %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.),
         type = "micronix") %>%
  relocate(id, .before = type) %>%
  relocate(specimen_id, .before = type) %>%
  select(-c(comments))

for (i in 1:nrow(storage_container)){
  
  info_list <- list(created = storage_container[i,]$"created",
                    last_updated = storage_container[i,]$"last_updated",
                    specimen_id = storage_container[i,]$"specimen_id",
                    type = storage_container[i,]$"type",
                    exhausted = storage_container[i,]$"exhausted")
  
  sampleDB::AddToTable(database = "/databases/sampledb/scratch/sampledb_database.sqlite",
                       table_name = "storage_container",
                       info_list = info_list)
}

# specimens - c("created",  "last_updated", "id",  "study_subject_id", "specimen_type_id", "collection_date")
old.specimen <- sampleDB::CheckTable(database = old_sdb, "specimen")

specimen <- old.specimen %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = study_subject_id)

AddTblToDB(table.name = "specimen", table.obj = specimen)

# subjects - c("created",  "last_updated", "id",  "study_id", "subject")
old.study_subject <- sampleDB::CheckTable(database = old_sdb, "study_subject")

study_subject <- old.study_subject %>%
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  relocate(id, .before = uid) %>%
  rename(subject = uid) %>%
  relocate(study_id, .before = subject)

AddTblToDB(table.name = "study_subject", table.obj = study_subject)

# ----- external
# matrix_plate - c("created",  "last_updated", "id",  "location_id", "plate_name")
old.matrix_plate <- sampleDB::CheckTable(database = old_sdb, "matrix_plate")

matrix_plate <- old.matrix_plate %>% 
  mutate(last_updated = last_updated %>% gsub("\\..*","",.),
         created = created %>% gsub("\\..*","",.)) %>%
  rename(plate_name = uid) %>%
  select(-c(hidden)) %>%
  relocate(location_id, .before = plate_name) %>%
  relocate(id, .before = location_id)

AddTblToDB(table.name = "matrix_plate", table.obj = matrix_plate)

# matrix_tube - c("id", "plate_id", "barcode", "well_position")
old.matrix_tube <- sampleDB::CheckTable(database = old_sdb, "matrix_tube")

matrix_tube <- old.matrix_tube

AddTblToDB(table.name = "matrix_tube", table.obj = matrix_tube)

# ------ user messages
print(paste0("migrated v0.0.1 ", "(", old_sdb, ")", " -> ", "v0.0.2 ", "(", new_sdb, ")"))
