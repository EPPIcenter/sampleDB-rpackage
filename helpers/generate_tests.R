


# GENERATE UPLOADCSVS
tbl.base <- tibble(study_subject_id = paste("subject", 0:9, sep = "_"), 
                   study_short_code = rep("KAM06", 10), 
                   specimen_type = rep("PLASMA", 10))

# GENERATE MATRIX UPLOADCSV
tbl.matrix_1 <- tibble(LocationRow = rep("A", 10),
                       LocationColumn = c(0:9),
                       TubeCode = paste("XXXX", 0:9),
                       collection_date = paste0("2022-11-", 1:10))
write.csv(cbind(tbl.matrix_1, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_1.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "micronix", upload_file = "~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_1.csv", 
                        container_name = "plate_1",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

tbl.matrix_2 <- tibble(LocationRow = rep("A", 10), 
                       LocationColumn = c(0:9),
                       TubeCode = paste("XXXX", 10:19),
                       collection_date = paste0("2022-11-", 11:20))
write.csv(cbind(tbl.matrix_2, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_2.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "micronix", upload_file = "~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_2.csv", container_name = "plate_2",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))


tbl.matrix_3 <- tibble(LocationRow = rep("A", 10), 
                       LocationColumn = c(0:9),
                       TubeCode = paste("XXXX", 20:29),
                       collection_date = paste0("2022-11-", 11:20))
write.csv(cbind(tbl.matrix_3, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_3.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "micronix", upload_file = "~/lib/packages/sampleDB-rpackage/tests/matrix/tbl.matrix_3.csv", container_name = "plate_3",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

# GENERATE CRYO UPLOADCSV

tbl.cryo_1 <- tibble(row = rep("A", 10), 
                     column = c(0:9),
                     label = paste("PPPP", 0:9),
                     collection_date = paste0("2022-11-", 1:10))
write.csv(cbind(tbl.cryo_1, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/cryovial/tbl.cryo_1.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "cryo",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/cryovial/tbl.cryo_1.csv",
                        container_name = "box_1",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

tbl.cryo_2 <- tibble(row = rep("A", 10), 
                     column = c(0:9),
                     label = paste("PPPP", 10:19),
                     collection_date = paste0("2022-11-", 11:20))
write.csv(cbind(tbl.cryo_2, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/cryovial/tbl.cryo_2.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "cryo",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/cryovial/tbl.cryo_2.csv",
                        container_name = "box_2",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

# GENERATE RDT UPLOADCSV
tbl.rdt_1 <- tibble(label = paste("MMMM", 0:9),
                    collection_date = paste0("2022-11-", 1:10))
write.csv(cbind(tbl.rdt_1, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/rdt/tbl.rdt_1.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "rdt",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/rdt/tbl.rdt_1.csv",
                        container_name = "bag_1",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

tbl.rdt_2 <- tibble(label = paste("MMMM", 10:19),
                    collection_date = paste0("2022-11-", 11:20))
write.csv(cbind(tbl.rdt_2, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/rdt/tbl.rdt_2.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "rdt",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/rdt/tbl.rdt_2.csv",
                        container_name = "bag_2",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

# GENERATE PAPER UPLOADCSV
tbl.paper_1 <- tibble(label = paste("QQQQ", 0:9),
                      collection_date = paste0("2022-11-", 1:10))
write.csv(cbind(tbl.paper_1, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/paper/tbl.paper_1.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "paper",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/paper/tbl.paper_1.csv",
                        container_name = "bag_3",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))

tbl.paper_2 <- tibble(label = paste("QQQQ", 10:19),
                      collection_date = paste0("2022-11-", 11:20))
write.csv(cbind(tbl.paper_2, tbl.base), paste0("~/lib/packages/sampleDB-rpackage/tests/paper/tbl.paper_2.csv"), row.names=FALSE)
sampleDB::UploadSamples(sample_type = "paper",
                        upload_file = "~/lib/packages/sampleDB-rpackage/tests/paper/tbl.paper_2.csv",
                        container_name = "bag_4",
                        freezer = list(location_name = "Freezer A",
                                             level_I = "A_dummy.level_I",
                                             level_II = "A_dummy.level_II"))
##########################

#NOTE how can these moves go wrong?
#esp bc cryovile moves will not be computer generated files...duplicate rows in move csv could exist

# MOVE MATRIX SAMPLES
swap_1 <- tbl.matrix_1[6, c("LocationColumn", "TubeCode")]
swap_2 <- tbl.matrix_2[6, c("LocationColumn", "TubeCode")]
tbl.matrix_1[6, c("LocationColumn", "TubeCode")] <- swap_2
tbl.matrix_2[6, c("LocationColumn", "TubeCode")] <- swap_1

write.csv(file = "~/lib/packages/sampleDB-rpackage/tests/matrix/move_csvs/m1.csv", tbl.matrix_1 %>% select(-c("collection_date")), row.names = F)
write.csv(file = "~/lib/packages/sampleDB-rpackage/tests/matrix/move_csvs/m2.csv", tbl.matrix_2 %>% select(-c("collection_date")), row.names = F)

sampleDB::MoveTubes(type = "matrix", file.container_samples = list("plate_1" = "~/m2.csv", "plate_2" = "~/m1.csv"))

# MOVE CRYO SAMPLES
tbl.cryo_1 %>% select(-c("collection_date"))
tbl.cryo_2 %>% select(-c("collection_date"))

sampleDB::MoveTubes(type = "cryo",
                    file.container_samples = list("box_1" = "~/",
                                                  "box_2" = "~/"))
# MOVE RDT SAMPLES
tbl.rdt_1 %>% select(-c("collection_date"))
tbl.rdt_2 %>% select(-c("collection_date"))

sampleDB::MoveTubes(type = "rdt",
                    file.container_samples = list("bag_1" = "~/",
                                                  "bag_2" = "~/"))
# MOVE PAPER SAMPLES
tbl.paper_1 %>% select(-c("collection_date"))
tbl.paper_2 %>% select(-c("collection_date"))

sampleDB::MoveTubes(type = "paper",
                    file.container_samples = list("bag_3" = "~/",
                                                  "bag_4" = "~/"))