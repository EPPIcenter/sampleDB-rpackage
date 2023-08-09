#' Upload Controls
#'
#' `UploadControls()` can be used to upload controls to the sampleDB database.
#'
#' @param control_type A string specifying the type of samples that are being uploaded Options include: `micronix`, `cryovial` and `dbs`
#' @param user_data A dataframe of SampleDB Upload data.\cr
#' Required `upload_data` columns are:\cr
#' `position`: the row and column of the sample in the storage housing
#' `label`: the sample's label or barcode
#' `study_subject_id`: the StudySubject id of for the subject in the cohort (ie study)
#' `study_short_code`: the code of the study
#' `specimen_type`: the sample type
#' `collection_date`: (optional) the date the sample was first collected from the cohort StudySubject
#'
#' #' **upload data example without collection_date**
#'
#' | position | label | study_subject_id | specimen_type | study_short_code |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            |
#'
#' **upload data example with collection_date**
#'
#' | position | label | study_subject_id | specimen_type | study_short_code | collection_data |
#' | ------------- | ----- | ---------------- | ------------- | ---------------- | --------------- |
#' | A0            | xxx1  | subject_1        | PLASMA        | KAM06            | 2022-04-11      |
#' | A1            | xxx2  | subject_2        | PLASMA        | KAM06            | 2022-04-11      |
#'
#' @param container_name A string specifying the name of the container the samples are in. Names must be unique within each sample type.
#' @param container_barcode A string specifying the barcode for the container the samples are in. Container barcodes are optional. Barcodes must be unique within each sample type.
#' @param freezer_address A list specifying the freezer address used to store samples. \cr
#' Required items in the freezer_address list are `name`, `level_I` and `level_II`.
#' If the freezer_address type is `minus eighty` then `level_I` and `level_II` items specify the rack and position, respecively.
#' If the freezer_address type is `minus twenty` then `level_I` and `level_II` items specify the shelf and basket, respecively.
#' @examples
#' \dontrun{
#' UploadSamples(sample_type = "micronix",
#'               upload_data = tibble(position = c("A0"),
#'                                    label = c("XXX 1"),
#'                                    study_subject_id = c("1"),
#'                                    specimen_type = c("PLASMA"),
#'                                    study_short_code = c("KAM06"),
#'                                    collection_date = c("2021-04-10")),
#'                container_name = "test_container",
#'                freezer_address = list(name = "TBD",
#'                                       level_I = "TBD",
#'                                       level_II = "seve's working basket"))
#' }
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#' @export
#'
UploadControls <- function(user_data, control_type, database = Sys.getenv("SDB_PATH")) {

	tryCatch({
		if (control_type=="dbs_sheet") {
			n.uploaded = UploadDBSSheet(user_data, database)
			message(sprintf("Uploaded %d DBS Sheet Controls!", n.uploaded))
		} else if (control_type =="whole_blood") {
			n.uploaded = UploadWholeBlood(user_data, database) 
			message(sprintf("Uploaded %d Whole Blood Controls!", n.uploaded))
		} else {
			stop("No implementation found for the specified control type")
		}
	},
	error = function(e) {
		message(e$message)
	})
}


#' Upload DBS Sheet Controls
#'
#' `UploadDBSSheet()` can be used to upload controls to the sampleDB database.
#'
#' @param con A dplyr dbConnect() connection object
#' @param user_data A dataframe of SampleDB Upload data.
#'
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#'
UploadWholeBlood <- function(user_data, database) {
	con <- DBI::dbConnect(RSQLite::SQLite(), database)

	dbBegin(con)

	now = as.character(lubridate::now())
	n.uploaded=0

	tryCatch({

		# calculate how many strains are in each recording

		user_data %>%
			group_by(density,strain,percentage,study_short_code) %>%


		df.payload = user_data %>%
			group_by(density,strain,percentage,study_short_code) %>%
			left_join(dbReadTable(con, "study_subject") %>%
			            dplyr::rename(
			              study_subject_id=id,
			              control=name
			            )
			          , by = c("control")) %>%
			  left_join(dbReadTable(con, "study") %>%
			               dplyr::rename(
			                 study_short_code=short_code,
			                 batch_id=id
			               )
			             , by = c("study_short_code")) %>%
				mutate(created = now) %>%
				mutate(last_updated = now) %>%
			  select(created, last_updated, control, batch_id, study_subject_id, all_of(colnames(user_data))) %>%
			  dplyr::rename(study_id=batch_id) %>%
			  ungroup()

		## Add the new study subjects here
		res <- dbAppendTable(con, "study_subject", df.payload %>%
			filter(is.na(study_subject_id)) %>%
			select(created, last_updated, study_id, control) %>%
			distinct() %>%
			dplyr::rename(name = control))

		## Rejoin to get the study_subject_id
		df.payload = df.payload %>%
		  select(-c(study_subject_id, created, last_updated)) %>%
			inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id), by = c("control"="name", "study_id")) %>%
		  select(created,last_updated,control,study_subject_id,all_of(colnames(user_data)))


		## Find the malaria controls and see if they exist already
		df.payload = df.payload %>%
			dplyr::left_join(dbReadTable(con, "malaria_blood_control") %>% 
				dplyr::rename(malaria_blood_control_id=id), by = c("density", "study_subject_id")) %>%
			dplyr::mutate(density = as.integer(density))

		## Add the blood controls if they do not exist
		res <- dbAppendTable(con, "malaria_blood_control", df.payload %>% 
			filter(is.na(malaria_blood_control_id)) %>%
			select(density, study_subject_id) %>%
			distinct()
		)


		## Find the locations and cryovial box if it already exists
		df.payload = df.payload %>%
		  inner_join(dbReadTable(con, "location") %>%
		               select(-c(created, last_updated)) %>%
		               dplyr::rename(location_id=id)
		             , by = c("location_root", "level_I", "level_II")) %>%
		  dplyr::left_join(dbReadTable(con, "cryovial_box") %>%
		                     select(-c(created, last_updated)) %>%
		                     dplyr::rename(cryovial_box_id=id, cryovial_box_barcode=barcode), by=c("manifest_name"="name", "location_id")) %>%
		  select(created,last_updated,control,study_subject_id,location_id,cryovial_box_id, all_of(colnames(user_data)))


		## if the box does not exist (cryovial_box_id == `NA`), then create it
		res <- dbAppendTable(con, "cryovial_box", df.payload %>%
		                       filter(is.na(cryovial_box_id)) %>%
		                       select(created, last_updated, location_id, manifest_name) %>%
		                       dplyr::rename(name = manifest_name) %>%
		                       distinct())


		## Rejoin to get the cryovial box ids
		df.payload=dbReadTable(con, "cryovial_box") %>% dplyr::rename(cryovial_box_id=id, cryovial_box_barcode=barcode) %>%
		  inner_join(df.payload %>% select(-c(created,last_updated,cryovial_box_id)), by = c("location_id", "name"="manifest_name")) %>%
		  dplyr::rename(manifest_name=name) %>%
		  select(created,last_updated,control, study_subject_id,location_id,cryovial_box_id,all_of(colnames(user_data)))


		## Add the whole blood tubes
		res <- dbAppendTable(con, "whole_blood_tube", df.payload %>%
							   dplyr::rename(malaria_blood_control_id=study_subject_id) %>%
		                       select(barcode, cryovial_box_id, position, malaria_blood_control_id) %>%
		                       distinct())

		## Update the table that links control density to strain, and records the percentage of that strain in the control
		df.payload = df.payload %>%
			mutate(
			  strain2 = strsplit(strain, ";"),
			  percentage2 = strsplit(percentage, ";")
			) %>%
			tidyr::unnest(cols = c("strain2","percentage2")) %>%
			left_join(dbReadTable(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain2"="strain")) %>%
			left_join(dbReadTable(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id=id), by = c("study_subject_id", "density"))

		res <- dbAppendTable(con, "composition_strain", df.payload %>% select(malaria_blood_control_id, strain_id, percentage2) %>% dplyr::rename(percentage=percentage2))
			
		dbCommit(con)
		n.uploaded=sum(nrow(user_data))

	}, error = function(e) {
		message(e$message)
	}, finally = {
		if (!is.null(con)) {
			dbDisconnect(con)
		}
	})
}

#' Upload DBS Sheet Controls
#'
#' `UploadDBSSheet()` can be used to upload controls to the sampleDB database.
#'
#' @param con A dplyr dbConnect() connection object
#' @param user_data A dataframe of SampleDB Upload data.
#'
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#'
UploadDBSSheet <- function(user_data, database) {

	con <- DBI::dbConnect(RSQLite::SQLite(), database)

	dbBegin(con)

	now = as.character(lubridate::now())
	n.uploaded=0

	## In the database, controls are stored as "study_subjects". This allows for extractions to be linked back to the original control.
	## With the current database schema, we need to uniquely identify each study_subject under a single study. Instead of changing this rule,
	## use the density, strain and percentage to start, and then index the control between [1:count]. If for whatever reason there are
	## controls that exist under this study already, then "base.count" will be used to offset the indexing so that each appended number
	## is +1 the second largest number under the study (ie. we always increment by 1).
	tryCatch({

		df.payload = user_data %>%
		  group_by(density,strain,percentage,study_short_code) %>%
		  tidyr::unite(control, c(density,strain,percentage), remove = FALSE) %>% # ex. 1K_W2;DD2;FCR3_20;20;60
	    left_join(dbReadTable(con, "study_subject") %>%
	                dplyr::rename(
	                  study_subject_id=id,
	                  control=name
	                )
	              , by = c("control")) %>%
		  left_join(dbReadTable(con, "study") %>%
		               dplyr::rename(
		                 study_short_code=short_code,
		                 batch_id=id
		               )
		             , by = c("study_short_code")) %>%
			mutate(created = now) %>%
			mutate(last_updated = now) %>%
		  select(created, last_updated, control, batch_id, study_subject_id, all_of(colnames(user_data))) %>%
		  dplyr::rename(study_id=batch_id) %>%
		  ungroup()


		## Add the new study subjects here
		res <- dbAppendTable(con, "study_subject", df.payload %>%
			filter(is.na(study_subject_id)) %>%
			select(created, last_updated, study_id, control) %>%
			distinct() %>%
			dplyr::rename(name = control))

		## Rejoin to get the study_subject_id
		df.payload = df.payload %>%
		  select(-c(study_subject_id, created, last_updated)) %>%
			inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id), by = c("control"="name", "study_id")) %>%
		  select(created,last_updated,control,study_subject_id,all_of(colnames(user_data)))

		## Find the locations and bags and join
		df.payload = df.payload %>%
		  inner_join(dbReadTable(con, "location") %>%
		               select(-c(created, last_updated)) %>%
		               dplyr::rename(location_id=id)
		             , by = c("location_root", "level_I", "level_II")) %>%
		  dplyr::left_join(dbReadTable(con, "dbs_bag") %>%
		                     select(-c(created, last_updated)) %>%
		                     dplyr::rename(dbs_bag_id=id), by=c("manifest_name"="name", "location_id")) %>%
		  select(created,last_updated,control,study_subject_id,location_id,dbs_bag_id, all_of(colnames(user_data)))

		## if the bag does not exist (dbs_bag_id == `NA`), then create it
		res <- dbAppendTable(con, "dbs_bag", df.payload %>%
		                       filter(is.na(dbs_bag_id)) %>%
		                       select(created, last_updated, location_id, manifest_name) %>%
		                       dplyr::rename(name = manifest_name) %>%
		                       distinct())

		## Rejoin to get the bag ids
		df.payload=dbReadTable(con, "dbs_bag") %>% dplyr::rename(dbs_bag_id=id) %>%
		  inner_join(df.payload %>% select(-c(created,last_updated,dbs_bag_id)), by = c("location_id", "name"="manifest_name")) %>%
		  dplyr::rename(manifest_name=name) %>%
		  select(created,last_updated,control, study_subject_id,location_id,dbs_bag_id,all_of(colnames(user_data)))

		## Find the malaria controls and see if they exist already
		df.payload = df.payload %>%
			dplyr::left_join(dbReadTable(con, "malaria_blood_control") %>% 
				dplyr::rename(malaria_blood_control_id=id), by = c("density", "study_subject_id")) %>%
			dplyr::mutate(density = as.integer(density))

		## Add the blood controls if they do not exist
		res <- dbAppendTable(con, "malaria_blood_control", df.payload %>% 
			filter(is.na(malaria_blood_control_id)) %>%
			select(density, study_subject_id) %>%
			distinct()
		)

		## Add control sheets here with their identifier (label) and the bag the exist in. If the sheet already exists, bump up the count.
		df.payload = df.payload %>%
			left_join(dbReadTable(con, "dbs_control_sheet") %>% dplyr::rename(dbs_control_sheet_id=id), by=c("dbs_bag_id", "label")) %>%
			select(created,last_updated,dbs_control_sheet_id,study_subject_id,dbs_bag_id,all_of(colnames(user_data)))


		## Add new control sheets. We know they are new because no id exists for them.
		res = dbAppendTable(con, "dbs_control_sheet", df.payload %>%
			filter(is.na(dbs_control_sheet_id)) %>%
			group_by(dbs_bag_id, label) %>%
			dplyr::mutate(replicates = n()) %>% ## This also handles the case where there are 2 sheets with the same name that are uploaded
			select(dbs_bag_id,label,replicates) %>%
			distinct())


		## Add the blood spot collections
		df.payload = df.payload %>%
			dplyr::left_join(dbReadTable(con, "blood_spot_collection") %>% 
				dplyr::rename(blood_spot_collection_id=id), by = c("study_subject_id"))

		res <- dbAppendTable(con, "blood_spot_collection", df.payload %>%
							   filter(is.na(blood_spot_collection_id)) %>%
								group_by(study_short_code,strain,percentage,density) %>%
											dplyr::mutate(
												count = as.integer(count),
												total=ifelse(is.na(total), 0, as.integer(total)),
												count = sum(count) + total
											) %>%
											select(-c(total)) %>%
											ungroup() %>%
											distinct() %>%
					                       dplyr::rename(total=count) %>%
					                       select(study_subject_id, total)
		)

		df.payload = df.payload %>%
			select(-c(blood_spot_collection_id)) %>%
			dplyr::inner_join(dbReadTable(con, "blood_spot_collection") %>% 
				select(-c(total, exhausted)) %>%
				dplyr::rename(blood_spot_collection_id=id), by = c("study_subject_id"))


		## Rejoin to get the new IDs
		df.payload = df.payload %>%
			select(-c(dbs_control_sheet_id)) %>%
			inner_join(dbReadTable(con, "dbs_control_sheet") %>% dplyr::rename(dbs_control_sheet_id=id), by=c("dbs_bag_id", "label")) %>%
			select(created,last_updated,blood_spot_collection_id,dbs_control_sheet_id,study_subject_id,dbs_bag_id,all_of(colnames(user_data)))

		## At this point, collapse the user data as we're no longer intereste edin retain specific sheet information
		df.payload = df.payload %>% distinct()

		## Update the junction table storing which bags blood collections can be found in. This is done so that blood collections from a batch
		## can span over multiple bags, and bags can contain mulitple types of blood collections (from the same batch or otherwise (this would
		## be a validation piece)).
		df.payload = df.payload %>%
			left_join(dbReadTable(con, "blood_spot_collection_dbs_control_sheet") %>% dplyr::rename(blood_spot_collection_dbs_control_sheet_id=id), by = c("blood_spot_collection_id", "dbs_control_sheet_id")) %>%
			select(created,last_updated,blood_spot_collection_id,dbs_control_sheet_id,blood_spot_collection_dbs_control_sheet_id,dbs_bag_id,study_subject_id,all_of(colnames(user_data)))

		res <- dbAppendTable(con, "blood_spot_collection_dbs_control_sheet", df.payload %>% 
			filter(is.na(blood_spot_collection_dbs_control_sheet_id)) %>%
			select(blood_spot_collection_id, dbs_control_sheet_id)
		)

		## Update the table that links control density to strain, and records the percentage of that strain in the control
		df.payload = df.payload %>%
			mutate(
			  strain2 = strsplit(strain, ";"),
			  percentage2 = strsplit(percentage, ";")
			) %>%
			tidyr::unnest(cols = c("strain2","percentage2")) %>%
			left_join(dbReadTable(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain2"="strain")) %>%
			left_join(dbReadTable(con, "malaria_blood_control") %>% dplyr::rename(malaria_blood_control_id=id), by = c("study_subject_id", "density"))

		res <- dbAppendTable(con, "composition_strain", df.payload %>% select(malaria_blood_control_id, strain_id, percentage2) %>% dplyr::rename(percentage=percentage2))
			
		dbCommit(con)
		n.uploaded=sum(as.integer(user_data$count))

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


#' UploadCompositions
#'
#' @param database Path to the sampleDB database
#' @param user_data Users data
#' @noRd
#' @export
UploadCompositions <- function(user_data, database = Sys.getenv("SDB_PATH")) {

	browser()
	
	# filter and arrange the composition table. we'll filter by using the 
	# calculated strain count from the user data, and then arrange so
	# that the percentages are ordered. if the largest percentage equals 
	# the largest percentage found in the user file, check that the strain matches, and if not, keep 
	# checking the strains until it's found. If the strain is not found or the percentage did not
	# match from before, terminate early because we now know this is a new composition. 

	# expected columns are strain, percentage and legacy.
	# strain and percentage will always have data, legacy can have `NA`.
	con = dbConnect(SQLite(), database)

	tryCatch({

		# modify the user data so that it can be comapared with strains
		user_data = user_data %>%
			dplyr::mutate(rownumber=RowNumber()) %>%
	    	dplyr::mutate(
	    		strain=strsplit(strain, ";"),
	    		percentage=strsplit(percentage, ";")
	    	) %>%
	    	tidyr::unnest(cols=c("strain", "percentage"))

		copy_to(con, user_data)

		# arrange the user data so that it is longform. to keep track
		# of the compositions, add a rownumber beforehand to group on. this
		# will also be used to easily count he number of strains in the composition.
		sql.user = tbl(con, "user_data") %>%
			group_by(rownumber) %>%
			dplyr::mutate(n_strain = n()) %>%
			ungroup()

		# filter and arrange the composition table to see if the composition
		# already exists. first, filtering will be done on the strain count, which
		# is kept in the composition table label column. 
		df = tbl(con, "composition_strain") %>%
			dplyr::rename(composition_strain_id=id) %>%
			dplyr::left_join(tbl(con, "composition") %>% dplyr::rename(composition_id=id), by=c("composition_id")) %>%
			dplyr::left_join(tbl(con, "strain") %>% dplyr::rename(strain=name, strain_id=id), by=c("strain_id")) %>% #  join the strain table to be used with user data
			group_by(composition_id) %>%
			mutate(n_strain=n()) %>%
			right_join(sql.user, by=c("n_strain", "strain", "percentage", "legacy")) %>%
			filter(is.na(composition_id)) %>%
			select(all_of(colnames(user_data)), n_strain, index) %>%
			collect() %>%
			dplyr::mutate(label=ifelse(!is.na(legacy), legacy, sprintf("S%d", n_strain))) %>% # `label` is S# for new compositions, `legacy` (from user file)
			dplyr::mutate(legacy=is.na(legacy)) # `legacy` is TRUE or FALSE (repurposing the column)
			dplyr::group_by(label) %>% # this is go calculate the index. For the legacy values, the behavior will be that the index will be 1.
			dplyr::mutate(
				max.recorded.index=ifelse(is.na(index), 0, max(index)), # get the largest index for this strain count type
				sum.new.n.strain = sum(!legacy), # indexing new compositions, so filter out legacy labels
				index=seq(max.recorded.index, max.recorded.index + sum.new.n.strain)
			) %>%
			# Create the data for the composition table (index, label, legacy)
			# To create the index, we need to grab the max(index) for each label, and then 
			# create a sequence up to the number of new indexes that will be added
			select(index, label, legacy) %>%
			distinct() %>%
			collect()


		# if there are new compositions, add them here
		res = dbAppendTable(con, "composition", df.payload)

		# Add the compostion_strain. start by rejoining against the composition table to get the new composition_ids. Next,
		# add the 
		df.payload = dbReadTable(con, "composition") %>%
			dplyr::rename(composition_id=id) %>%
			dplyr::inner_join(df, by = c("index", "label", "legacy")) %>%
			select(composition_id, strain_id, percentage)

		res = dbAppendTable(con, "composition_strain", df.payload)

		dbCommit(con)

	},
	error = function(e) {
		message(e$message)
	},
	finally = {
		if (!is.null(con)) {
			dbDisconnect(con)
		}
	})
	return (nrow(user_data))
}