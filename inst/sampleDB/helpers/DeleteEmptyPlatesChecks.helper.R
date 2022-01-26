


DeleteEmptyPlateRequirement <- function(input, database){
  #GET PLATE ID FROM PLATE NAME
  id.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == input$DeletePlateName)$id
  
  #GET NUMBER OF TUBES ASSOCIATED WITH PLATE ID
  num_tubes_in_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == id.plate) %>% nrow()
  
  #REQUIRE THAT THE NUMBER OF TUBES ASSOCUATED WITH PLATE ID IS ZERO
  out <- req(num_tubes_in_plate == 0)
  return(out)
}

helper.CheckDeleteEmptyPlate <- function(input, database){

  if(input$DeletePlateName != ""){
    id.plate <- filter(CheckTable(database = database, "matrix_plate"), uid == input$DeletePlateName)$id
    num_tubes_in_plate <- filter(CheckTable(database = database, "matrix_tube"), plate_id == id.plate) %>% nrow()
    out <- validate(need(num_tubes_in_plate == 0, "Cannot delete plate. Plate is not empty"))
  }else{
    out <- NULL
  }

  return(out)
}