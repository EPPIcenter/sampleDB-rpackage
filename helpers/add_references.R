

# UPDATE FREEZER REFERENCE

for(i in 1:5){
  print(i)
  for(j in 1:4){
    print(j)
    sampleDB::UpdateReferences(reference = "freezer",
                               operation = "add",
                               update = list(freezer_name = "countess",
                                             freezer_type = "minus_twenty",
                                             freezer_levelI = paste0("shelf_", i),
                                             freezer_levelII = paste0("basket_", j)))
  }
}

sampleDB::UpdateReferences(reference = "freezer",
                           operation = "add",
                           update = list(freezer_name = "TBD",
                                         freezer_type = "minus20",
                                         freezer_levelI = "i",
                                         freezer_levelII = "j"))

sampleDB::UpdateReferences(reference = "freezer",
                           operation = "modify",
                           identifier = list(freezer_name = "Freezer B",
                                             freezer_levelI = "B_dummy.level_I",
                                             freezer_levelII = "B_dummy.level_II"),
                           update = list(freezer_name = "Freezer A",
                                         freezer_type = "A",
                                         freezer_levelI = "A_dummy.level_I",
                                         freezer_levelII = "A_dummy.level_II"))

sampleDB::UpdateReferences(reference = "freezer",
                           operation = "delete",
                           identifier = list(freezer_name = "Freezer B",
                                             freezer_levelI = "B_dummy.level_I",
                                             freezer_levelII = "B_dummy.level_II"))

# UPDATE SPECIMEN TYPE REFERENCE

sampleDB::UpdateReferences(reference = "specimen_type",
                           operation = "add",
                           update = list(specimen_type_name = "PLASMAc"))

sampleDB::UpdateReferences(reference = "specimen_type",
                           operation = "modify",
                           identifier = list(specimen_type_name = "PLASMA"),
                           update = list(specimen_type_name = "PLERSMER"))

sampleDB::UpdateReferences(reference = "specimen_type",
                           operation = "delete",
                           identifier = list(specimen_type_name = "PLERSMER"))


# UPDATE STUDY REFERENCE

sampleDB::UpdateReferences(reference = "study",
                           operation = "add",
                           update = list(study_title = "Title KAM06",
                                         study_description = "Test Study",
                                         study_short_code = "KAM06",
                                         study_longitudinal = FALSE,
                                         study_lead_person = "Seve"))

sampleDB::UpdateReferences(reference = "study",
                           operation = "modify",
                           identifier = list(study_short_code = "KAM06"),
                           update = list(study_title = "Title KAM06",
                                         study_description = "Test Study",
                                         study_short_code = "KAM06",
                                         study_longitudinal = 0,
                                         study_lead_person = "Seve"))

sampleDB::UpdateReferences(reference = "study",
                           operation = "delete",
                           identifier = list(DeleteStudyShortCode = "KAM06"))