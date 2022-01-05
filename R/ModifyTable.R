#' @import dplyr
#' @import RSQLite
#' @export

ModifyTable <- function(database, table_name, info_list, id){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database)

  #PREVENT EMPTY ADDITIONS TO DATABASE -- REMOVE NAs FROM THIS EVALUATION
  for(i in discard(info_list, is.na)){
    if(as.character(i) == ""){
      return()
    }
  }

  update_str <- c()
  for(i in 1:length(info_list)){
    if(i != length(info_list)){
      update_str <- c(update_str, names(info_list)[i], " = ", "'", info_list[[i]], "', ")
    }else{
      update_str <- c(update_str, names(info_list)[i], " = ", "'", info_list[[i]], "'")
    }
  }
  update_str <- update_str %>% paste0(., collapse = "")

  tryCatch(
    RSQLite::dbSendQuery(conn,
                         paste0("UPDATE ", table_name," SET ", update_str," WHERE id = ", id, ";")),
    error=function(e){
      if(e[1] == "UNIQUE constraint failed: location.description"){
        return(warning("FREEZER NAMES CANNOT BE DUPLICATED"))
      }
    }
  )

  RSQLite::dbDisconnect(conn)
}
