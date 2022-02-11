#' Search for Wetlab Samples in the EPPIcenter sampleDB database
#' 
#' @param filters A list containing micronix barcodes, plate name, study code, location and/or specimen_type
#' @param study_subject.file TRUE or FALSE
#' @examples
#' name.location = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII")
#' SearchSamples(filters = list(name.plate = c("100","101"), name.location = c("Left -20 Freezer")))
#' SearchSamples(filters = list(file.barcodes = "/path/to/barcodes.csv", name.plate = "dummy_name", name.study = "dummy_study", name.location = "fridge", name.specimen_type = "specimen_type1"))
#' sampleDB::SearchSamples(filters = list(name.location = list(location_name = "Freezer A", level_I = "dummy.levelI", level_II = "dummy.levelII"))) # feb 10 2022
#' @import dplyr
#' @import RSQLite
#' @import emojifont
#' @import purrr
#' @import readr
#' @import tidyr
#' @export

SearchSamples <- function(filters, study_subject.file = F){
  
  database <- "/databases/new.sampleDB.db"
  
  if(study_subject.file){
    eval.name.study_subject  <- read.csv(filter$name.study_subject)$subject_uid
    filters$name.study_subject <- eval.name.study_subject[eval.name.study_subject != ""] # remove any blank entries that may be in vector
  }

  # GET ALL THE TABLES FROM THE DATABASE
  table.storage_container <- sampleDB::CheckTable(database = database, "storage_container")
  table.study_subject <- sampleDB::CheckTable(database = database, "study_subject")
  table.specimen <- sampleDB::CheckTable(database = database, "specimen")
  table.study <- sampleDB::CheckTable(database = database, "study")
  table.location <- sampleDB::CheckTable(database = database, "location")
  table.specimen_type <- sampleDB::CheckTable(database = database, "specimen_type")
  table.box <- sampleDB::CheckTable(database = database, "box")
  table.tube <- sampleDB::CheckTable(database = database, "tube")
  table.bag <- sampleDB::CheckTable(database = database, "bag")
  table.rdt <- sampleDB::CheckTable(database = database, "rdt")
  table.paper <- sampleDB::CheckTable(database = database, "paper")
  table.plate <- sampleDB::CheckTable(database = database, "matrix_plate")
  table.matrix_tube <- sampleDB::CheckTable(database = database, "matrix_tube")


  #COLLECT SEARCH AND FILTERING TERMS - FIRST ITEM NOT "" IS SEARCH TERM THE REST ARE FILTER TERMS
  term.search <- discard(filters, function(x) is.null(x))[1] %>% names()
  terms.filter <- names(discard(filters, function(x) is.null(x)))[-1]
  
  if(is.na(term.search)){
    search_results <- NULL
  }else{

    # USE STUDY TO GET STORAGE CONTAINER ID
    if(term.search == "name.study"){
      study_ref_id <- filter(table.study, short_code %in% filters$name.study)$id
      study_subject_ref_id <- filter(table.study_subject, study_id %in% study_ref_id)$id
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
    }
    
    # USE SPECIMEN TYPE TO GET STORAGE CONTAINER ID
    if(term.search == "name.specimen_type"){
      specimen_ref_id <- filter(table.specimen_type, label %in% filters$name.specimen_type)$id
      specimen_ref_id <- filter(table.specimen, specimen_type_id %in% specimen_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
    }
    # USE SUBJECT TO GET STORAGE CONTAINER ID
    if(term.search == "name.study_subject"){
      print(filters$name.study_subject)
      study_subject_ref_id <- filter(table.study_subject, subject %in% filters$name.study_subject)$id
      print(study_subject_ref_id)
      specimen_ref_id <- filter(table.specimen, study_subject_id %in% study_subject_ref_id)$id
      storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ref_id)$id
      print(storage_container_id)
    }
    # USE LOCATION LIST TO GET STORAGE CONTAINER ID
    if(term.search == "name.location"){
      # need freezer_name, level_I, level_II
      # location_ref_id <- inner_join(table.location,
      #                               tibble(location_name = filters$name.location$location_name, level_I = filters$name.location$level_I , level_II = filters$name.location$level_II),
      #                               by = c("location_name", "level_I", "level_II"))$id
      
      # make sure you can search by just one aspect of locations in the filters$name.location(ie freezer name, l1, l2), and that that search can include multiple names (l1 = c("A_rack", "B_rack"))
      t <- table.location
      if(!is.null(filters$name.location$location_name)){
        t <- filter(t, location_name %in% filters$name.location$location_name)
      }
      if(!is.null(filters$name.location$level_I)){
        t <- filter(t, level_I %in% filters$name.location$level_I)
      }
      if(!is.null(filters$name.location$level_II)){
        t <- filter(t, level_II %in% filters$name.location$level_II)
      }
      location_ref_id <- t$id
      print(location_ref_id)

      # tubes.storage_container_id <- rdt.storage_container_id <- paper.storage_container_id <- matrix.storage_container_id <- NULL
      
      tubes_id <- filter(table.tube, box_id %in% filter(table.box, location_id %in% location_ref_id)$id)$id
      # if(nrow(filter(table.storage_container, type == "cryo") > 0)){
      #   cryo.storage_container <- filter(table.storage_container, type == "cryo") %>% mutate(n = 1:n())
      #   tubes.storage_container_id <- filter(cryo.storage_container, n %in% tubes_id)$id
      # }
      rdt_id <- filter(table.rdt, bag_id %in% filter(table.bag, location_id %in% location_ref_id)$id)$id
      # if(nrow(filter(table.storage_container, type == "rdt") > 0)){
      #   rdt.storage_container <- filter(table.storage_container, type == "rdt") %>% mutate(n = 1:n())
      #   rdt.storage_container_id <- filter(rdt.storage_container, n %in% tubes_id)$id
      # }
      paper_id <- filter(table.paper, bag_id %in% filter(table.bag, location_id %in% location_ref_id)$id)$id
      # if(nrow(filter(table.storage_container, type == "paper") > 0)){
      #   paper.storage_container <- filter(table.storage_container, type == "paper") %>% mutate(n = 1:n())
      #   paper.storage_container_id <- filter(paper.storage_container, n %in% tubes_id)$id 
      # }
      matrix_tubes_id <- filter(table.matrix_tube, plate_id %in% filter(table.plate, location_id %in% location_ref_id)$id)$id
      # if(nrow(filter(table.storage_container, type == "micronix") > 0)){
      #   matrix.storage_container <- filter(table.storage_container, type == "micronix") %>% mutate(n = 1:n())
      #   matrix.storage_container_id <- filter(matrix.storage_container, n %in% tubes_id)$id
      # }
      
      # storage_container_id <- c(tubes.storage_container_id, rdt.storage_container_id, paper.storage_container_id, matrix.storage_container_id)
      storage_container_id <- filter(table.storage_container, id %in% c(tubes_id, rdt_id, paper_id, matrix_tubes_id))$id
    }
    # USE DATE TO GET STORAGE CONTAINER ID
    if(term.search == "dates"){
     d1 <- lubridate::as_date(filter$date$d1)
     d2 <- lubridate::as_date(filter$date$d2)
     specimen_ids <- filter(table.specimen, lubridate::as_date(collection_date) %within% interval(d1,d2))$id
     storage_container_id <- filter(table.storage_container, specimen_id %in% specimen_ids)$id
    }
    # USE TYPE TO GET STORAGE CONTAINER ID -- 
    if(term.search == "type"){
      storage_container_id <- filter(table.storage_container, type %in% filters$type)$id
    }
    # USE TYPE SPECIFIC THINGS TO FILTER
    
    # if(term.search == "file.barcodes"){
    #   barcodes <- read_csv(filters$file.barcodes)$barcode
    #   matrix_tube_ids <- filter(table.matrix_tube, barcode %in% barcodes)$id
    # }
    # if(term.search == "name.plate"){
    #   #message: name.plates not in table
    #   plate_ref_id <-  filter(table.plate, plate_name %in% filters$name.plate)$id
    #   matrix_tube_ids <- filter(table.matrix_tube, plate_id %in% plate_ref_id)$id
    # }
     
    #NOTE:DO WE WANT TO ADD MORE INFO TO SEARCH RESULTS TABLE (E.G. LEAD STUDY PERSON)
    #USE MATRIX_TUBE_IDS TO GET THE ITEMS FOR THE SEARCH RESULT TABLE
    
    # table.ref1 <-  filter(table.matrix_tube, id %in% matrix_tube_ids)
    # output.well_positions <- table.ref1$well_position
    # output.barcodes <- table.ref1$barcode
    # 
    # table.ref2 <- inner_join(table.ref1, table.plate, by = c("plate_id" = "id"))
    # output.plate_uids <- table.ref2$plate_name
    # output.location_location_names <- inner_join(table.ref2, table.location, by = c("location_id" = "id"))$location_name
    # specimen_ids <- filter(table.storage_container, id %in% matrix_tube_ids)
    
    specimen_ids <- filter(table.storage_container, id %in% storage_container_id)
    table.ref3 <- inner_join(specimen_ids, table.specimen, by = c("specimen_id" = "id"))
    study_subject_id <- table.ref3$study_subject_id
    specimen_type_ids <- table.ref3$specimen_type_id
    output.collection_date <- table.ref3$collection_date

    table.ref4 <- inner_join(table.ref3, table.study_subject, by = c("study_subject_id" = "id"))
    output.subject_uids <- table.ref4$subject
    output.study_short_code <- inner_join(table.ref4, table.study, by = c("study_id" = "id"))$short_code
  
    output.specimen_type_labels <- inner_join(table.ref3, table.specimen_type, by = c("specimen_type_id" = "id"))$label
    
    #now get external data
    tube_dat <- inner_join(filter(table.tube, id %in% storage_container_id), 
                           table.box[, c("id","box_name", "location_id")], 
                           by = c("box_id" = "id")) %>%
      select(-c(box_id)) %>%
      rename(container_position = box_position,
             container_name = box_name) %>%
      mutate(type = "Micronix")

    micr_dat <- inner_join(filter(table.matrix_tube, id %in% storage_container_id), 
                           table.plate[, c("id","plate_name", "location_id")], 
                           by = c("plate_id" = "id")) %>% 
      select(-c(plate_id)) %>%
      rename(container_position = well_position,
             container_name = plate_name,
             label = barcode) %>%
      mutate(type = "Micronix")

    rdt_dat <- inner_join(filter(table.rdt, id %in% storage_container_id), 
                          table.bag[, c("id","bag_name", "location_id")], 
                          by = c("bag_id" = "id")) %>% 
      select(-c(bag_id)) %>%
      mutate(type = "RDT",
             container_position = NA) %>%
      rename(container_name = bag_name)

    paper_dat <- inner_join(filter(table.paper, id %in% storage_container_id), 
                            table.bag[, c("id","bag_name", "location_id")], 
                            by = c("bag_id" = "id")) %>% 
      select(-c(bag_id)) %>%
      mutate(type = "Paper",
             container_position = NA) %>%
      rename(container_name = bag_name)
    
    # add freezer data to external date
    external_data <- rbind(tube_dat, micr_dat, rdt_dat, paper_dat)
    external_data <- inner_join(external_data, table.location, by = c("location_id" = "id")) %>%
      select(-c("created", "last_updated", "id"))
      
    #STITCH TOGETHER SEARCH RESULTS
    search_results <- tibble(
                             subject_uid = output.subject_uids %>% as.factor(),
                             study = output.study_short_code %>% as.factor(),
                             specimen_type = output.specimen_type_labels %>% as.factor(),
                             collection_date = lubridate::as_date(output.collection_date),
                             container_name = external_data$container_name %>% as.factor(),
                             container_position = external_data$container_position %>% as.factor(),
                             label = external_data$label,
                             type = external_data$type %>% as.factor(),
                             freezer = external_data$location_name %>% as.factor(),
                             freezer_l1 = external_data$level_I %>% as.factor(),
                             freezer_l2 = external_data$level_II %>% as.factor()
                             )
    
    # USE TYPE SPECIFIC THINGS TO FILTER
    if(length(terms.filter) > 0){
      #FILTER BY FILTER TERMS
      for(filter_term in terms.filter){
        if(filter_term == "name.study"){
          search_results <- filter(search_results, study %in% filters[["name.study"]])
        }
        if(filter_term == "name.specimen_type"){
          search_results <- filter(search_results, specimen_type %in% filters[["name.specimen_type"]])
        }
        if(filter_term == "name.study_subject"){
          search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        }
        # if(filter_term == "name.date"){
        #   search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        # }
        # if(filter_term == "name.container_name"){
        #   search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        # }
        # if(filter_term == "name.type"){
        #   search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        # }
        # if(filter_term == "name.location"){
        #   search_results <- filter(search_results, subject_uid %in% filters[["name.study_subject"]])
        # }
      } 
    }
    
    # CLEAN SEARCH TABLE
    search_results <- search_results %>%
      # relocate("plate_uid") %>%
      rename(`Sample Type` = type,
             `Container Name` = container_name,
             `Container Position` = container_position,
             `Label` = label,
             `Study Subject` = subject_uid,
             `Study Code` = study,
             `Specimen Type` = specimen_type,
             `Storage Location` = freezer,
             `Storage Location.level_I` = freezer_l1,
             `Storage Location.level_II` = freezer_l2,
             `Collected Date` = collection_date)
    # %>%
    #   mutate(letter = substring(well_position, 1, 1),
    #          number = as.numeric(substring(well_position, 2))) %>%
    #   select(-well_position) %>%
    #   group_by(`Plate Name`, `Barcode`, letter, number) %>%
    #   arrange(`Plate Name`, letter, number) %>%
    #   ungroup() %>%
    #   mutate(`Well Position` = paste0(letter, number), .after = `Plate Name`) %>%
    #   select(-c(letter, number))
    
    if(nrow(search_results) == 0){
      # message("THERE ARE NO EPPICENTER WETLAB SAMPLES THAT MATCH THIS SEARCH")
      search_results <- NULL
    }
  }
  return(search_results)
}
