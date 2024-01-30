#' Upload Controls to the Database
#'
#' This function uploads control data to the specified database based on the control type.
#' Currently supports uploading for 'dbs_sheet' and 'whole_blood' control types.
#'
#' @param user_data Data to be uploaded. It should match the expected format for the specified control type.
#' @param control_type The type of control to be uploaded. Valid values are 'dbs_sheet' and 'whole_blood'.
#' @param database The path to the database. Defaults to the system environment variable "SDB_PATH".
#'
#' @return NULL. However, the function provides messages to inform the user about the number of uploaded controls or any encountered errors.
#'
#' @examples
#' # Assuming you have a data frame called df_dbs_sheet and df_whole_blood:
#' upload_controls(df_dbs_sheet, control_type = "dbs_sheet")
#' upload_controls(df_whole_blood, control_type = "whole_blood")
#'
#' @export
upload_controls <- function(user_data, control_type, database = Sys.getenv("SDB_PATH")) {

	tryCatch({
		if (control_type == "dbs_sheet") {
			n.uploaded = upload_dbs_sheet(user_data, database)
			message(sprintf("Uploaded %d DBS Sheet Controls!", n.uploaded))
		} else if (control_type == "whole_blood") {
			n.uploaded = upload_whole_blood(user_data, database)
			message(sprintf("Uploaded %d Whole Blood Controls!", n.uploaded))
		} else {
			stop("No implementation found for the specified control type")
		}
	},
	error = function(e) {
		message(e$message)
	})
}




#' Process Whole Blood Box Data
#' 
#' This function will check for locations and add new boxes if they don't exist. It will return the boxes and locations IDs.
#' 
#' @param user_data A dataframe containing the payload data.
#' @param con A database connection object.
#' @param created_col Column name in user_data for created date.
#' @param last_updated_col Column name in user_data for last updated date.
#' @param box_name_col Column name in user_data for box name.
#' @param box_barcode_col Column name in user_data for box barcode.
#' @return A dataframe with the location ids and cryovial box ids.
process_whole_blood_location_container <- function(user_data, con, created_col, last_updated_col, box_name_col, box_barcode_col, location_root_col, level_I_col, level_II_col) {

  # get the boxes ids
  user_data <- join_locations_and_boxes(con, user_data, box_name_col, box_barcode_col, location_root_col, level_I_col, level_II_col)

  # add the boxes if they don't exist
  res <- append_boxes_if_not_exist(con, user_data, created_col, last_updated_col, box_name_col, box_barcode_col)
  cat("Whole Blood Boxes Added: ", res, "\n")

  # rejoin to get the box ids
  user_data <- rejoin_box_ids(con, user_data, box_name_col, box_barcode_col)

  return(user_data)
}

#' Process Whole Blood Box Data
#' 
#' This function will check for locations and add new boxes if they don't exist. It will return the boxes and locations IDs.
#' 
#' @param con A database connection object.
#' @param user_data A dataframe containing the payload data.
#' @param box_name_col Column name in user_data for box name.
#' @param box_barcode_col Column name in user_data for box barcode.
#' 
join_locations_and_boxes <- function(con, user_data, box_name_col, box_barcode_col, location_root_col, level_I_col, level_II_col) {

  joins <- setNames(
    c("name", "location_id", "cryovial_box_barcode"),
    c(box_name_col, "location_id", box_barcode_col)
  )

  location_joins <- setNames(
    c("location_root", "level_I", "level_II"),
    c(location_root_col, level_I_col, level_II_col)
  )

  ## Find the locations and cryovial box if it already exists
  df <- user_data %>%
    inner_join(dbReadTable(con, "location") %>%
                  select(-c(created, last_updated)) %>%
                  dplyr::rename(location_id=id)
                , by = location_joins) %>%
    dplyr::left_join(dbReadTable(con, "cryovial_box") %>%
                        select(-c(created, last_updated)) %>%
                        dplyr::rename(
                          cryovial_box_id=id,
                          cryovial_box_barcode=barcode
                        ), by=joins) %>%
    select(location_id,cryovial_box_id, all_of(colnames(user_data)))

  return(df)

}

#' Rejoin Whole Blood Box Data
#' 
#' @param con A database connection object.
#' @param user_data A dataframe containing the payload data.
#' @param box_name_col Column name in user_data for box name.
#' @param box_barcode_col Column name in user_data for box barcode.
#' @return A dataframe with the location ids and cryovial box ids.
#' @keywords internal
rejoin_box_ids <- function(con, user_data, box_name_col, box_barcode_col) {

  joins <- setNames(
    c("name", "location_id", "barcode"),
    c(box_name_col, "location_id", box_barcode_col)
  )

  df <- user_data %>%
    dplyr::select(-c(cryovial_box_id)) %>%
    inner_join(dbReadTable(con, "cryovial_box") %>%
                  dplyr::rename(cryovial_box_id=id), by = joins) %>%
    select(location_id, cryovial_box_id, all_of(colnames(user_data)))

  return(df)

}

#' Add whole blood tubes to the database if they don't exist
#' 
#' @param user_data A dataframe containing the payload data.
#' @param con A database connection object.
#' @param barcode_col Column name in user_data for barcode.
#' @param position_col Column name in user_data for position.
#' @return A result from the dbAppendTable indicating if the tubes were added successfully.
append_whole_blood_tubes <- function(user_data, con, barcode_col, position_col) {
  ## Add the whole blood tubes
  res <- dbAppendTable(con, "whole_blood_tube", user_data %>%
                dplyr::rename(position := !!sym(position_col)) %>%
                select(any_of(c(barcode_col)), malaria_blood_control_id, cryovial_box_id, position) %>%
                distinct()
  )

  return(res)
}

#' Process Whole Blood Box Data
#' 
#' This function will check for locations and add new boxes if they don't exist. It will return the boxes and locations IDs.
#' 
#' @param con A database connection object.
#' @param user_data A dataframe containing the payload data.
#' @param created_col Column name in user_data for created date.
#' @param last_updated_col Column name in user_data for last updated date.
#' @param box_name_col Column name in user_data for box name.
#' @param box_barcode_col Column name in user_data for box barcode.
#' 
#' @return A result from the dbAppendTable indicating if the boxes were added successfully.
#' @keywords internal
append_boxes_if_not_exist <- function(con, user_data, created_col, last_updated_col, box_name_col, box_barcode_col) {

  ## if the box does not exist (cryovial_box_id == `NA`), then create it
  res <- dbAppendTable(con, "cryovial_box", user_data %>%
                          filter(is.na(cryovial_box_id)) %>%
                          select(!!sym(created_col), !!sym(last_updated_col), location_id, !!sym(box_name_col), !!sym(box_barcode_col)) %>%
                          dplyr::rename(created = created_col, last_updated = last_updated_col, name = box_name_col, barcode = box_barcode_col) %>%
                          distinct())

  return(res)

}

upload_whole_blood <- function(user_data, database) {
  con <- DBI::dbConnect(RSQLite::SQLite(), database)

	dbBegin(con)

  res <- NULL

	now = as.character(lubridate::now())
	n.uploaded=0

	tryCatch({
    user_data <- prepare_control_for_upload(user_data, now)

    user_data_with_control_ids <- create_controls_for_batch(user_data, con, "Density", "Batch", "Control")
    user_data_with_container_ids <- process_whole_blood_location_container(user_data_with_control_ids, con, "Created", "LastUpdated", "BoxName", "BoxBarcode", "WB_Minus80", "WB_RackName", "WB_RackPosition")
    user_data_with_blood_control_ids <- process_malaria_blood_control_data(user_data_with_container_ids, con, "Density", "CompositionID")
    res <- append_whole_blood_tubes(user_data_with_blood_control_ids, con, "Barcode", "ControlOriginPosition")
		dbCommit(con)
	}, error = function(e) {
    dbRollback(con)
		message(e$message)
	}, finally = {
		if (!is.null(con)) {
			dbDisconnect(con)
		}
	})

	## Return number of dbs controls uploaded - this will be the aggregated counts.
	return(res)
}




#' Create Controls for the batch
#'
#' This function prepares user data, appends study subjects to a database table, and rejoins to get the study_subject_id.
#'
#' @param user_data A dataframe containing user data to process.
#' @param con A database connection object.
#' @param density_col Column name in user_data for density value.
#' @param study_short_code_col Name of the column in user_data corresponding to "Batch".
#' @param control_col Name of the column in user-dta correspond to "Control" (system made)
#' @return A dataframe with the study_subject_id added.
#' @export
create_controls_for_batch <- function(
  user_data,
  con,
  density_col,
  study_short_code_col,
  control_col
) {

  # Join conditions
  # Directly define the join conditions using named vectors
  study_subject_joins <- setNames(
    c("name", "study_id"),
    c(control_col, "study_id")
  )

  study_joins <- setNames(
  	c("short_code"),
  	c(study_short_code_col)
  )

  # Preparing data
  df.payload <- user_data %>%
    group_by(
      !!sym(density_col),
      !!sym(study_short_code_col)
    ) %>%
    left_join(
      dbReadTable(con, "study"),
      by = study_joins
    ) %>%
    dplyr::rename(study_id = id) %>%
    left_join(
      dbReadTable(con, "study_subject"),
      by = study_subject_joins
    ) %>%
    dplyr::rename(study_subject_id=id) %>%
    select(
      study_id, study_subject_id, all_of(colnames(user_data))
    ) %>%
    ungroup()

	# Appending to the database
	res <- dbAppendTable(con, "study_subject", df.payload %>%
	                      filter(is.na(study_subject_id)) %>%
	                      select(Created, LastUpdated, study_id, !!sym(control_col)) %>%
	                      distinct() %>%
	                      dplyr::rename(
                          name = !!sym(control_col),
                          created = Created,
                          last_updated = LastUpdated))

	rejoin_by <- setNames(
		c("name", "study_id"),
		c(control_col, "study_id")
	)

	## Rejoin to get the study_subject_id
	df.payload = df.payload %>%
	  select(-c(study_subject_id)) %>%
		inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id), by = rejoin_by) %>%
	  select(study_subject_id, all_of(colnames(user_data)))

  return(df.payload)
}


#' Join locations and bags from user data to the database
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param location_root_col Column name in df.payload for the root location.
#' @param level_I_col Column name in df.payload for level I location.
#' @param level_II_col Column name in df.payload for level II location.
#' @param manifest_name_col Column name in df.payload for manifest name.
#' @return A dataframe with joined location and bag details.
join_locations_and_bags <- function(df.payload, con, location_root_col, level_I_col, level_II_col, manifest_name_col) {

  location_joins <- setNames(
    c("location_root", "level_I", "level_II"),
    c(location_root_col, level_I_col, level_II_col)
  )

  bag_joins <- setNames(
    c("name", "location_id"),
    c(manifest_name_col, "location_id")
  )

  df <- df.payload %>%
    inner_join(
      dbReadTable(con, "location") %>%
      select(-c(created, last_updated)) %>%
      dplyr::rename(location_id = id),
      by = location_joins
    ) %>%
    left_join(
      dbReadTable(con, "dbs_bag") %>%
      select(-c(created, last_updated)) %>%
      dplyr::rename(dbs_bag_id = id),
      by = bag_joins
    ) %>%
    select(location_id, dbs_bag_id, all_of(colnames(df.payload)))

  return(df)
}

#' Add new bags to the database if they don't exist
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param manifest_name_col Column name in df.payload for manifest name.
#' @return A result from the dbAppendTable indicating if the bags were added successfully.
add_bags_if_not_exist <- function(df.payload, con, manifest_name_col) {
  res <- dbAppendTable(con, "dbs_bag", df.payload %>%
    filter(is.na(dbs_bag_id)) %>%
    dplyr::select(location_id, Created, LastUpdated, all_of(manifest_name_col)) %>%
    dplyr::rename(
      name = all_of(manifest_name_col),
      created = Created,
      last_updated = LastUpdated
    ) %>%
    distinct())
  return(res)
}

#' Rejoin the payload with the database to retrieve bag IDs
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param manifest_name_col Column name in df.payload for manifest name.
#' @return A dataframe with updated bag IDs.
rejoin_to_get_bag_ids <- function(df.payload, con, manifest_name_col) {
  df <- dbReadTable(con, "dbs_bag") %>%
    dplyr::rename(dbs_bag_id = id) %>%
    inner_join(
      df.payload %>% select(-c(dbs_bag_id)),
      by = c("location_id", "name" = manifest_name_col)
    ) %>%
    dplyr::rename(!!manifest_name_col := "name") %>%
    select(dbs_bag_id, all_of(colnames(df.payload)))

  return(df)
}

#' Process user data to handle bag details
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param location_root_col Column name in df.payload for the root location.
#' @param level_I_col Column name in df.payload for level I location.
#' @param level_II_col Column name in df.payload for level II location.
#' @param manifest_name_col Column name in df.payload for manifest name.
#' @return A dataframe with processed bag details.
process_bag_data <- function(df.payload, con, location_root_col, level_I_col, level_II_col, manifest_name_col) {

  # Join with locations and bags
  df.payload <- join_locations_and_bags(df.payload, con, location_root_col, level_I_col, level_II_col, manifest_name_col)

  # Add bags if they don't exist and update the connection result
  res <- add_bags_if_not_exist(df.payload, con, manifest_name_col)

  # Rejoin to get the bag IDs
  df.payload <- rejoin_to_get_bag_ids(df.payload, con, manifest_name_col)

  return(df.payload)
}

#' Join malaria controls based on density and study subject ID
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @return A dataframe with joined malaria controls.
join_malaria_controls <- function(df.payload, con) {

  joins <- setNames(
    c("study_subject_id", "composition_id"),
    c("study_subject_id", "composition_id")
  )

  df <- df.payload %>%
    left_join(
      dbReadTable(con, "malaria_blood_control") %>%
        dplyr::rename(malaria_blood_control_id = id),
      by = joins
    ) %>%
    select(malaria_blood_control_id, all_of(colnames(df.payload)))

  return(df)
}

#' Rejoin malaria controls based on density and study subject ID
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @return A dataframe with joined malaria controls.
rejoin_malaria_controls <- function(df.payload, con, density_col) {
  joins <- setNames(
    c("study_subject_id", "composition_id", "density"),
    c("study_subject_id", "composition_id", density_col)
  )

  df <- df.payload %>%
    select(-c(malaria_blood_control_id)) %>%
    inner_join(
      dbReadTable(con, "malaria_blood_control") %>%
        dplyr::rename(malaria_blood_control_id = id),
      by = joins
    ) %>%
    select(malaria_blood_control_id, all_of(colnames(df.payload)))

  return(df)
}

#' Add malaria blood controls to the database if they don't exist
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param density User Data Density Column
#' @return A result from the dbAppendTable indicating if the controls were added successfully.
add_malaria_blood_controls_if_not_exist <- function(df.payload, con, density_col) {
  res <- dbAppendTable(con, "malaria_blood_control", df.payload %>%
    filter(is.na(malaria_blood_control_id)) %>%
    select(study_subject_id, density_col, composition_id) %>%
    distinct()
  )
  return(res)
}

#' Process user data to handle malaria blood control details
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param composition_id_col Column representing CompositionID.
#' @return A dataframe with processed malaria blood control details.
join_composition_ids <- function(df.payload, con, composition_id_col) {

  # Retrieve compositions using the provided function
  compositions <- retrieve_compositions_by_identifier(con, df.payload[[composition_id_col]])
  denorm_compositions <- denormalize_composition_ids(compositions, "label", "index", "legacy", composition_id_col)
  
  # Join compositions with payload data
  df <- df.payload %>%
    left_join(
      denorm_compositions %>%
        dplyr::rename(composition_id = id),
      by = setNames(composition_id_col, composition_id_col)
    ) %>%
    select(composition_id, all_of(colnames(df.payload)))

  # Assuming you want to keep all columns from df.payload and any newly joined ones
  return(df)
}

#' Process user data to handle malaria blood control details
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param density_col Column name in df.payload for density value.
#' @param composition_id_col Column representing CompositionID.
#' @return A dataframe with processed malaria blood control details.
process_malaria_blood_control_data <- function(df.payload, con, density_col, composition_id_col) {
  # Join with malaria controls
	df.payload <- join_composition_ids(df.payload, con, composition_id_col)

  df.payload <- join_malaria_controls(df.payload, con)

  # Add malaria blood controls if they don't exist
  res <- add_malaria_blood_controls_if_not_exist(df.payload, con, "Density")

  cat("Malaria Blood Controls Added: ", res, "\n")

  df.payload <- rejoin_malaria_controls(df.payload, con, density_col)

  return(df.payload)
}

#' Join DBS control sheets based on bag ID and label
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param dbs_bag_id_col Column name in df.payload for DBS bag ID.
#' @param label_col Column name in df.payload for label.
#' @return A dataframe with joined DBS control sheets.
join_dbs_control_sheets <- function(df.payload, con, sheet_name_col) {

  joins <- setNames(
    c("dbs_sheet_label", "dbs_bag_id"),
    c(sheet_name_col, "dbs_bag_id")
  )

  df <- df.payload %>%
    left_join(
      dbReadTable(con, "dbs_control_sheet") %>%
        dplyr::rename(
          dbs_control_sheet_id = id,
          dbs_sheet_label = label
        ),
      by = joins
    ) %>%
    select(dbs_control_sheet_id, all_of(colnames(df.payload)))

  return(df)
}

#' Add new DBS control sheets to the database
#'
#' @param con A database connection object.
#' @param df.payload A dataframe with payload data.
#' @param dbs_sheet_name_col Column name in df.payload for DBS Sheet name.
#' @return A result from the dbAppendTable indicating if the sheets were added successfully.
add_new_dbs_control_sheets <- function(con, df.payload, dbs_sheet_name_col) {
  res <- dbAppendTable(con, "dbs_control_sheet", df.payload %>%
    filter(is.na(dbs_control_sheet_id)) %>%
    group_by(dbs_bag_id, !!sym(dbs_sheet_name_col)) %>%
    dplyr::mutate(replicates = n()) %>%
    dplyr::select(dbs_bag_id, !!sym(dbs_sheet_name_col), replicates) %>%
    dplyr::rename(label = !!sym(dbs_sheet_name_col)) %>%
    select(dbs_bag_id, label, replicates) %>%
    distinct()
  )
  return(res)
}


#' Rejoin to Get New DBS Control Sheet IDs
#'
#' @param df.payload A dataframe containing payload data.
#' @param con A database connection object.
#' @return A dataframe after rejoining to get the new DBS control sheet IDs.
rejoin_dbs_control_sheet_ids <- function(df.payload, con, dbs_sheet_name_col) {
  joins <- setNames(
    c("dbs_bag_id", "label"),
    c("dbs_bag_id", dbs_sheet_name_col)
  )

  df <- df.payload %>%
    select(-c(dbs_control_sheet_id)) %>%
    inner_join(
      dbReadTable(con, "dbs_control_sheet") %>%
      dplyr::rename(dbs_control_sheet_id = id),
      by = joins
    ) %>%
    select(dbs_control_sheet_id, all_of(colnames(df.payload)))

  return(df)
}


#' Process user data to handle DBS control sheet details
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @param dbs_sheet_name_col Column name in df.payload for DBS Sheet name.
#' @return A dataframe with processed DBS control sheet details.
process_dbs_control_sheet_data <- function(df.payload, con, dbs_sheet_name_col) {
  # Join with DBS control sheets

  df.payload <- join_dbs_control_sheets(df.payload, con, dbs_sheet_name_col)

  # Add new DBS control sheets if they don't exist
  res <- add_new_dbs_control_sheets(con, df.payload, dbs_sheet_name_col)

  cat("DBS Control Sheets Added: ", res, "\n")

  # Rejoin to get the new DBS control sheet IDs
  df.payload <- rejoin_dbs_control_sheet_ids(df.payload, con, dbs_sheet_name_col)

  return(df.payload)
}

#' Join Blood Spot Collections
#'
#' @param df.payload A dataframe containing payload data.
#' @param con A database connection object.
#' @return A dataframe after joining with blood spot collections.
join_blood_spot_collections <- function(df.payload, con) {
  browser()
  df <- df.payload %>%
    left_join(
      dbReadTable(con, "blood_spot_collection") %>%
      dplyr::rename(blood_spot_collection_id = id),
      by = setNames(
        c("malaria_blood_control_id", "dbs_control_sheet_id"),
        c("malaria_blood_control_id", "dbs_control_sheet_id")
      )
    ) %>%
    select(blood_spot_collection_id, total, dbs_control_sheet_id, all_of(colnames(df.payload)))

  return(df)
}

#' Add New Blood Spot Collections
#'
#' @param df.payload A dataframe containing payload data.
#' @param con A database connection object.
#' @param count_col Column name in df.payload for count.
#' @param dbs_control_sheet_id Column name in df.payload for DBS control sheet ID.
#' @return Result from the dbAppendTable indicating if the collections were added successfully.
add_new_blood_spot_collections <- function(df.payload, con, count_col, dbs_control_sheet_id) {
  res <- dbAppendTable(con, "blood_spot_collection", df.payload %>%
    filter(is.na(blood_spot_collection_id)) %>%
    group_by(malaria_blood_control_id, dbs_control_sheet_id) %>%
    dplyr::mutate(
      count = as.integer(!!sym(count_col)),
      total = ifelse(is.na(total), 0, as.integer(total)),
      count = sum(count) + total
    ) %>%
    select(-c(total)) %>%
    ungroup() %>%
    select(malaria_blood_control_id, dbs_control_sheet_id, count) %>%
    distinct() %>%
    dplyr::rename(total = count)
  )

  return(res)
}

#' Re-join with updated blood spot collections
#'
#' This function updates the payload dataframe by rejoining with the updated blood spot collections.
#' It removes the original 'blood_spot_collection_id' from the payload and excludes 'total' and 'exhausted' columns 
#' from the blood spot collections during the join.
#'
#' @param df.payload A dataframe with payload data.
#' @param con A database connection object.
#' @return A dataframe with rejoined blood spot collections.
rejoin_with_updated_blood_spot_collections <- function(df.payload, con) {
  browser()
  df.updated <- df.payload %>%
    dplyr::select(-c(blood_spot_collection_id)) %>%
    dplyr::inner_join(
      dbReadTable(con, "blood_spot_collection") %>%
      dplyr::rename(blood_spot_collection_id = id) %>%
      dplyr::select(-c(total, exhausted)),
      by = setNames(
        c("malaria_blood_control_id", "dbs_control_sheet_id"),
        c("malaria_blood_control_id", "dbs_control_sheet_id")
      )
    ) %>%
    dplyr::select(blood_spot_collection_id, dbs_control_sheet_id, all_of(colnames(df.payload)))
  
  return(df.updated)
}

#' Process Blood Spot Collection Data
#'
#' @param df.payload A dataframe containing payload data.
#' @param con A database connection object.
#' @param count_col Count column.
#' @return A dataframe with processed blood spot collection details.
process_blood_spot_collection_data <- function(df.payload, con, count_col) {

  browser()

  # Join with blood spot collections
  df.payload <- join_blood_spot_collections(df.payload, con)

  # Add new blood spot collections if they don't exist
  res <- add_new_blood_spot_collections(df.payload, con, count_col, "dbs_control_sheet_id")

  cat("Blood Spot Collections Added: ", res, "\n")

  # Re-join with the updated blood spot collections
  df.payload <- rejoin_with_updated_blood_spot_collections(df.payload, con)

  return(df.payload)
}

#' Create Control Label with Density and Composition Values
#'
#' This function takes vectors for density and composition and creates a control label 
#' by combining the values. Densities greater than 999 are divided
#' by 1000 and appended with a 'K'.
#'
#' @param dens_values A numeric vector specifying the density values.
#' @param comp_values A character vector specifying the composition values.
#'
#' @return A character vector with the control labels.
#'
#' @examples
#' dens <- c(100, 1000, 1500)
#' comp <- c("S1_1", "S3_2", "S2_2")
#' create_control_label(dens, comp)
#'
#' @export
create_control_label <- function(dens_values, comp_values) {
  
  # Ensure the input vectors have the same length
  if(length(dens_values) != length(comp_values)) {
    stop("The lengths of 'dens_values' and 'comp_values' must be the same.")
  }
  
  # Convert densities
  converted_densities <- ifelse(
    dens_values > 999,
    paste0(dens_values / 1000, "K"),
    as.character(dens_values)
  )
  
  # Create control labels
  control_labels <- paste0(converted_densities, "_", comp_values)
  
  return(control_labels)
}

#' Prepare Control for Upload
#'
#' This function prepares a user_data dataframe for upload by adding a 'Control' 
#' column (generated from Density and CompositionID values), and by adding 'Created' 
#' and 'LastUpdated' columns with the current timestamp.
#'
#' @param user_data A dataframe with user data containing 'Density' and 'CompositionID' columns.
#' @param now A timestamp indicating the current time.
#'
#' @return A dataframe with additional columns 'Control', 'Created', and 'LastUpdated'.
#'
#' @examples
#' test_data <- data.frame(Density = c(100, 1000, 1500),
#'                         CompositionID = c("S1_1", "S3_2", "S2_2"))
#' prepare_control_for_upload(test_data, Sys.time())
#'
#' @export
prepare_control_for_upload <- function(user_data, now) {
  
  user_data[['Control']] <- create_control_label(user_data$Density, user_data$CompositionID)
  user_data[['Created']] <- now
  user_data[['LastUpdated']] <- now

  return(user_data)
}

upload_dbs_sheet <- function(user_data, database) {

	con <- DBI::dbConnect(RSQLite::SQLite(), database)

	dbBegin(con)

	now = as.character(lubridate::now())
	n.uploaded=0

	tryCatch({

    ## Prepare the user data for upload
    user_data <- prepare_control_for_upload(user_data, now)

		user_data_with_control_ids <- create_controls_for_batch(user_data, con, "Density", "Batch", "Control")
		user_data_with_bag_ids <- process_bag_data(user_data_with_control_ids, con, "DBS_FreezerName", "DBS_ShelfName", "DBS_BasketName", "SheetName")
		user_data_with_blood_control_ids <- process_malaria_blood_control_data(user_data_with_bag_ids, con, "Density", "CompositionID")
		user_data_with_sheet_ids <- process_dbs_control_sheet_data(user_data_with_blood_control_ids, con, "SheetName")
		user_data_with_blood_spot_collection_ids <- process_blood_spot_collection_data(user_data_with_sheet_ids, con, "Count")

		## At this point, collapse the user data as we're no longer intereste edin retain specific sheet information
		user_data_distinct <- user_data_with_blood_spot_collection_ids %>% distinct()

		dbCommit(con)
		n.uploaded=sum(as.integer(user_data[["Count"]]))

	}, error = function(e) {
		message(e$message)
	}, finally = {
		if (!is.null(con)) {
			dbDisconnect(con)
		}
	})

	## Return number of dbs controls uploaded - this will be the aggregated counts.
	return(n.uploaded)
}


#' Format labels for compositions in a dplyr pipeline
#'
#' This function formats the labels for the given compositions based on the legacy status and index.
#' The function is designed to work within a dplyr pipeline.
#'
#' @param data A tibble containing the compositions to be labeled.
#'        The dataframe should contain the columns `legacy`, `label`, and `index`.
#' @param legacy_col The name of the column that contains the legacy flag. Default is "legacy".
#' @param label_col The name of the column that contains the labels. Default is "label".
#' @param index_col The name of the column that contains the index. Default is "index".
#'
#' @return A tibble containing the formatted labels.
#' @export
#'
#' @examples
#' df <- tibble(legacy = c(1, 0), label = c("S1", "S2"), index = c(1, 2))
#' df %>% format_labels()
format_labels <- function(data, 
                          legacy_col = "legacy", 
                          label_col = "label", 
                          index_col = "index") {
  
  data %>%
    dplyr::mutate(
      !!label_col := case_when(
        (!is.na(!!sym(index_col))) ~ paste0(!!sym(label_col), "_", !!sym(index_col)),
        TRUE ~ !!sym(label_col)
      )
    )
}

#' Reshape Identifier Data to Long Form
#'
#' This function takes a data frame and reshapes it to long form based on the strain-percentage pairs 
#' in the 'unique_id' column. It separates each strain-percentage pair into two new columns: 'strain' 
#' and 'percentage'.
#'
#' @param data A data frame containing a 'unique_id' column with strain-percentage pairs, separated 
#'             by semicolons (';') and hyphens ('-').
#' @return A data frame reshaped to long form, containing new 'strain' and 'percentage' columns.
#' @importFrom tidyr separate_longer_delim separate_wider_delim
#' @export
reshape_identifier_to_long_form <- function(data) {
  # create long form data from identifier ({strain}-{percentage};{strain}-{percentage};...)
  long_data <- data %>%
    # Duplicate each row for each strain-percentage pair in 'unique_id'
    separate_longer_delim(unique_id, delim = ";") %>%
    # Separate 'unique_id' into 'strain' and 'percentage' columns
    separate_wider_delim(unique_id, names = c("strain", "percentage"), delim = "-") %>%
    # Convert 'percentage' to numeric
    dplyr::mutate(percentage = as.numeric(percentage))
  
  return(long_data)
}

#' Create a unique identifier for compositions
#'
#' This function creates a unique identifier for given strains and their respective percentages.
#' The strains and percentages are sorted based on percentages to ensure consistency in unique ID creation.
#'
#' @param strains A character vector containing strain names.
#' @param percentages A numeric vector containing percentages for each strain.
#'
#' @return A character string representing the unique identifier for the composition.
#'
#' @examples
#' strains <- c("D6", "3D7", "HB3")
#' percentages <- c(0.32, 0.33, 0.35)
#' create_unique_id(strains, percentages)
create_unique_id <- function(strains, percentages) {
  # Sort based on percentages
  order_idx <- order(percentages)
  sorted_strains <- strains[order_idx]
  sorted_percentages <- percentages[order_idx]

  # Create a unique identifier
  paste(paste(sorted_strains, sorted_percentages, sep = "-"), collapse = ";")
}

#' Split and sort strains and percentages
#'
#' This function splits strains and percentages strings and sorts them based on percentages.
#'
#' @param strains A character string with strains separated by semicolons.
#' @param percentages A character string with percentages separated by semicolons.
#'
#' @return A list containing sorted strains and sorted percentages.
split_and_sort <- function(strains, percentages) {
    # Split strains and percentages
    split_strains <- unlist(strsplit(strains, ";"))
    split_percentages <- as.numeric(unlist(strsplit(percentages, ";")))

    # Sort them based on percentages (ascending) and then strains (ascending)
    order_idx <- order(split_percentages, split_strains)
    list(sorted_strains = split_strains[order_idx], sorted_percentages = split_percentages[order_idx])
}

#' Create a unique identifier from sorted strains and percentages
#'
#' This function generates a unique identifier by combining sorted strains and percentages.
#'
#' @param sorted_strains A character vector of sorted strains.
#' @param sorted_percentages A numeric vector of sorted percentages.
#'
#' @return A character string representing the unique identifier.
create_unique_id_from_sorted <- function(sorted_strains, sorted_percentages) {
    paste(paste(sorted_strains, sorted_percentages, sep = "-"), collapse = ";")
}

#' Prepare new compositions for database upload
#'
#' Compositions consist of strains and percentages, where there can be multiple or single strains and the sum of each strain
#' must be close to 1 (some tolerance is allowed). The composition is a descriptive identifier in the case of new monoclonal
#' and polyclonal, where the monoclonal label is the name of the strain, and the polyclonal label is a formatted string of the number
#' of strains and a unique, incrementing index. For example, "W2" is a monoclonal 'W2' composition and "2S_1" is a dual strain composition
#' that we would need to collect more information about from the database. Monoclonal compositions are always considered legacy.
#'
#' @param user_data A data frame containing user compositions.
#'
#' @return A dataframe with updated compositions and legacy labels.
prepare_new_compositions <- function(user_data) {

  user_data %>%
    mutate(LegacyLabel = as.character(LegacyLabel),
           # Update legacy logic to include monoclonal strains (strain_count == 1)
           strain_count = lengths(strsplit(Strains, split=";")),
           legacy = LegacyLabel != "" & LegacyLabel != "NA" & !is.na(LegacyLabel) | strain_count == 1,
           index = if_else(!legacy & strain_count > 1, ave(strain_count, strain_count, FUN = seq_along, na.rm = TRUE), NA_integer_),
           label = case_when(
               legacy ~ if_else(strain_count == 1, as.character(Strains), LegacyLabel),
               strain_count > 1 ~ paste0("S", strain_count, "_", index),
               TRUE ~ NA_character_
           )
    )
}


#' Retrieve compositions from the database by identifier
#'
#' @description This function accepts an identifier that describes the composition of a control, and retrieves data that matches the identifier from the database.
#' 
#' @param con A database connection object.
#' @param identifiers A character vector of identifiers to be matched.
#'
#' @return A dataframe of matched compositions
retrieve_compositions_by_identifier <- function(con, identifiers) {
  
  # Apply the helper function to split all identifiers
  split_ids <- lapply(identifiers, split_composition_id)
  
  # Extract standard and non-standard identifiers based on the legacy flag
  standard_ids <- split_ids[!sapply(split_ids, `[[`, "legacy")]
  non_standard_ids <- split_ids[sapply(split_ids, `[[`, "legacy")]
  
  # Extract labels and indices for standard identifiers
  labels_standard <- sapply(standard_ids, `[[`, "label")
  indices_standard <- sapply(standard_ids, `[[`, "index")

  # Retrieve compositions for standard identifiers
  result_standard <- tbl(con, "composition") %>%
    dplyr::filter(label %in% labels_standard & index %in% indices_standard) %>%
    collect()

  # Extract labels for non-standard identifiers
  labels_non_standard <- sapply(non_standard_ids, `[[`, "label")
  
  # Retrieve compositions for non-standard identifiers
  result_non_standard <- tbl(con, "composition") %>%
    dplyr::filter(label %in% labels_non_standard) %>%
    collect()

  # Combine results
  result <- rbind(result_standard, result_non_standard)
  
  return(result)
}


#' Process and append new compositions to the database
#'
#' @param con A database connection object.
#' @param user_data A data frame containing user compositions.
#'
#' @return Integer number of rows appended.
process_and_append_compositions <- function(con, user_data) {
    new_compositions <- prepare_new_compositions(user_data)

    dbAppendTable(con, "composition", new_compositions %>% select(label, index, legacy))

    unique_labels <- unique(new_compositions$label)
    newly_added_compositions <- retrieve_compositions_by_label(con, unique_labels)

    composition_strain_data <- dplyr::inner_join(new_compositions, newly_added_compositions, by = c("index", "label", "legacy")) %>%
      select(id, Strains, Percentages)

    composition_strain_data_long <- split_and_unnest_columns(composition_strain_data, "Strains", "Percentages", append = "Long") %>%
        dplyr::rename(composition_id = id, strain = StrainsLong, percentage = PercentagesLong) %>%
        dplyr::left_join(dbReadTable(con, "strain") %>% dplyr::rename(strain_id = id), by = c("strain" = "name"))

    appended_rows <- dbAppendTable(con, "composition_strain", composition_strain_data_long %>% select(composition_id, strain_id, percentage))

    new_compositions_return <- new_compositions %>% select(label, index, legacy)
    return(new_compositions_return)
}


#' Upload compositions to the database
#'
#' This function uploads user data to the database after generating unique identifiers and matching them with existing entries.
#'
#' @param user_data A data frame containing user compositions.
#' @param database A character string representing the database path. Default is a system environment variable "SDB_PATH".
#'
#' @return A formatted list of labels representing the added or matched compositions.
#' @export
upload_compositions <- function(user_data, database = Sys.getenv("SDB_PATH")) {

    # Initialize the database connection
    con = init_db_conn(database)
    # Initialize a vector to hold labels
    all_labels <- character()
    tryCatch({

        # Extract unique identifiers from user data and the database
        user_data_identifiers <- get_unique_compositions_from_user_data(user_data)
        db_data_identifiers_updated <- get_unique_compositions_from_database(con)

        # Full join fuzzy merge, then filter (could add the 'inner join' to the function but this is fine.)
        merged_data <- fuzzy_merge_unique_compositions(user_data_identifiers, db_data_identifiers_updated) %>%
          rowwise() %>%
          filter(!is.null(sorted_percentages_user)) # keep the data we want (user data)!

        matched_user_data <- merged_data %>% filter(match)
        unmatched_user_data <- merged_data %>% filter(!match)

        # Continue if there are matching compositions
        if(nrow(matched_user_data) > 0) {
            # Format and append the labels of existing compositions
            existing_labels <- format_labels(matched_user_data)
            all_labels <- c(all_labels, existing_labels)
        }

        # If there are unmatched compositions
        if(nrow(unmatched_user_data) > 0) {
            # Start a transaction
            dbBegin(con)

            # Process and upload new compositions to the database
            user_compositions <- process_and_append_compositions(con, unmatched_user_data)

            # Append labels of the added compositions
            new_labels <- format_labels(user_compositions) 
            all_labels <- c(all_labels, new_labels$label)

	        # Commit changes to the database
	        dbCommit(con)
        }

        return(all_labels)
    }, error = function(e) {
        message(e$message)
        dbRollback(con)
    }, finally = {
        if (!is.null(con)) {
            dbDisconnect(con)
        }
    })

    return(all_labels)
}


#' Fuzzy Merge Unique Compositions
#'
#' This function performs a fuzzy merge of unique compositions based on sorted strains and percentages.
#' It compares the percentages within a given tolerance to decide whether they should be considered the same.
#' This function performs a full join without filtering, the user should filter afterwards if they want an 'inner join' behavior.
#'
#' @param user_data Data frame containing sorted_strains_key and sorted_percentages columns, typically the output from get_unique_compositions_from_user_data.
#' @param db_data Data frame containing sorted_strains_key and sorted_percentages columns, typically the output from get_unique_compositions_from_database.
#' @param tolerance Numeric tolerance within which two percentages are considered similar. Default is 0.01.
#' @return A data frame with a logical column 'match' indicating whether the compositions from user_data and db_data match within the given tolerance.
#' @export
fuzzy_merge_unique_compositions <- function(user_data, db_data, tolerance = 0.01) {
  
  # Perform a full join on sorted_strains_key
  merged_data <- dplyr::full_join(user_data, db_data, by = "sorted_strains_key", suffix = c("_user", "_db"))
  
  # Function to compare percentages within tolerance
  compare_percentages_within_tolerance <- function(p1, p2, tol) {
    if (is.null(p1) || is.null(p2)) {
      return(FALSE)
    }
    
    return(all(abs(p1 - p2) <= tol))
  }
  
  # Apply the comparison function to each row
  merged_data <- merged_data %>%
    rowwise() %>%
    mutate(match = compare_percentages_within_tolerance(sorted_percentages_user, sorted_percentages_db, tolerance)) %>%
    ungroup()

  return(merged_data)
}
