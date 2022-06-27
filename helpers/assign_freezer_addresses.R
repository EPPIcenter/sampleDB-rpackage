
library(googlesheets4)
library(sampleDB)

sdb_freezer_addresses <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1n2nGaTbjxW39i85Es_zAxvafXtGY6IOagZLGCKZlPkc/edit#gid=0") %>%
  mutate(basket = basket %>% unlist() %>% as.vector())

unique(sdb_freezer_addresses$basket)
# ^^^ make sure basket is either
# - a number ("60")
# - someone's working basket (of the form "<person>'s working basket")
# - "not yet in basket" of "NA"

#add an "unlocated" basket
if(!"unlocated" %in% CheckTable("location")$level_II){
  sampleDB::UpdateReferences(reference = "freezer",
                             operation = "add",
                             update = list(freezer_name = "TBD",
                                           freezer_type = "minus20",
                                           freezer_levelI = "TBD",
                                           freezer_levelII = "unlocated")) 
}

#add a basket as each lab member's working space
lab_members <- c("jessica", "jo", "jared", "sisi", "sophie", "eric", "andres", "bryan")
working_baskets <- paste0(lab_members, "'s working basket")
for(working_basket in working_baskets){
  if(!working_basket %in% CheckTable("location")$level_II){
    sampleDB::UpdateReferences(reference = "freezer",
                               operation = "add",
                               update = list(freezer_name = "TBD",
                                             freezer_type = "minus20",
                                             freezer_levelI = "TBD",
                                             freezer_levelII = working_basket)) 
  }
}


# for each container in the database
sdb_containers <- CheckTable("matrix_plate")
for(i in 1:nrow(sdb_containers)){
  
  #get container name and id
  sdb_plate_name <- sdb_containers[i,]$"plate_name"
  sdb_plate_id <- sdb_containers[i,]$"id"
  
  #get teresa's info in the container
  m <- which(sdb_freezer_addresses$`container_name (in sampledb)` == sdb_plate_name)
  basket_info <- sdb_freezer_addresses[m,]$"basket"
  
  #if container is in teresa's "container locator" file
  if(length(basket_info > 0)){
    
    # if container is in a ordinary basket
    if(!is.na(as.numeric(basket_info) %>% suppressWarnings())){
      basket_name <- paste("basket", basket_info, sep = "")
      
      # if the freezer address this ordinary container is in is not yet in the db
      if(!basket_name %in% CheckTable("location")$level_II){
        sampleDB::UpdateReferences(reference = "freezer",
                                   operation = "add",
                                   update = list(freezer_name = "TBD",
                                                 freezer_type = "minus20",
                                                 freezer_levelI = "TBD",
                                                 freezer_levelII = basket_name))
        freezer_address_id <- filter(CheckTable("location"), level_II == basket_name)$id
        ModifyTable(database = "/var/lib/sampleDB/sampledb_database.sqlite", table_name = "matrix_plate", info_list = list(location_id = freezer_address_id), id = sdb_plate_id)
      
      }else{
        freezer_address_id <- filter(CheckTable("location"), level_II == basket_name)$id
        ModifyTable(database = "/var/lib/sampleDB/sampledb_database.sqlite", table_name = "matrix_plate", info_list = list(location_id = freezer_address_id), id = sdb_plate_id)
      }
      
    # if container is NOT in an ordinary basket  
    }else{
      
      # if the container is ???
      if(basket_info == "NA" | basket_info == "not yet in basket"){
        # container is not in a basket
        freezer_address_id <- filter(CheckTable("location"), level_II == "unlocated")$id
        ModifyTable(database = "/var/lib/sampleDB/sampledb_database.sqlite", table_name = "matrix_plate", info_list = list(location_id = freezer_address_id), id = sdb_plate_id)
        
      # if the container is in someone's working basket
      }else{
        # container is in someone's working space
        if(basket_info %in% working_baskets){
          freezer_address_id <- filter(CheckTable("location"), level_II == basket_info)$id
          ModifyTable(database = "/var/lib/sampleDB/sampledb_database.sqlite", table_name = "matrix_plate", info_list = list(location_id = freezer_address_id), id = sdb_plate_id)
        }
      }
    }
    
  # if the container does not exist in teresa's "container locator file"  
  }else{
    freezer_address_id <- filter(CheckTable("location"), level_II == "unlocated")$id
    print(paste(basket_info,"put into teresa's locator file, bc this basket is not in there"))
    ModifyTable(database = "/var/lib/sampleDB/sampledb_database.sqlite", table_name = "matrix_plate", info_list = list(location_id = freezer_address_id), id = sdb_plate_id)
  }
}

#if all items from a plate are deleted, is the plate deleted
# ^yes, they are
#if i add new samples after samples have been deleted, does the sample id from the deleted samples get recycled?
# this would be bad bc it might make sample ids in matrix tube and storage container become out of step
# ^thankfully when adding new samples brand new ids are created, the ids asso w the deleted samples are not used
#durin a move can i add a plate that isnt in the database?
