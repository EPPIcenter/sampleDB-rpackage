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

UploadControls <- function(user_data, control_type, database = Sys.getenv("SDB_PATH")) {
	tryCatch({

		con <- DBI::dbConnect(RSQLite::SQLite(), database)

		dbBegin(con)
		now = as.character(lubridate::now())
		user_data=dplyr::rename(user_data, location_name=name)

		## In the database, controls are stored as "study_subjects". This allows for extractions to be linked back to the original control.
		## With the current database schema, we need to uniquely identify each study_subject under a single study. Instead of changing this rule,
		## use the density, strain and percentage to start, and then index the control between [1:count]. If for whatever reason there are
		## controls that exist under this study already, then "base.count" will be used to offset the indexing so that each appended number
		## is +1 the second largest number under the study (ie. we always increment by 1).

		df.payload = dbReadTable(con, "study") %>%
		  dplyr::rename(batch=short_code) %>%
			dplyr::mutate(study_id=id) %>%
			filter(!is.na(control_collection_id)) %>%
			dplyr::inner_join(user_data, by = c("batch")) %>%
		  dplyr::left_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id), by=c("study_id")) %>%
		  group_by(density,strain,percentage,batch) %>%
		  dplyr::mutate(base.count=sum(!is.na(study_subject_id))) %>%
		  dplyr::mutate(count = as.integer(count), count = sum(base.count + count)) %>%
		  distinct() %>%
		  dplyr::mutate(count = paste(1:count, collapse=";")) %>%
			dplyr::mutate(count = strsplit(count, ";")) %>%
			tidyr::unnest(cols=c("count")) %>%
		  ungroup() %>%
			dplyr::mutate(count=as.character(count)) %>%
			tidyr::unite(control, c(density,strain,percentage,count), remove = FALSE) %>%
		  mutate(created = now) %>%
		  mutate(last_updated = now) %>%
	    select(created, last_updated, control, study_id, all_of(colnames(user_data)))

		df.payload$sheet_id = user_data %>%
		  dplyr::mutate(count=as.integer(count)) %>%
		  dplyr::mutate(sheet_id=row_number()) %>%
		  tidyr::uncount(count) %>%
		  pull(sheet_id)

		## Add the new study subjects here
		res <- dbAppendTable(con, "study_subject", df.payload %>% select(created, last_updated, study_id, control) %>% dplyr::rename(name = control))

		## Rejoin to get the study_subject_id
		df.payload = df.payload %>%
			inner_join(dbReadTable(con, "study_subject") %>% dplyr::rename(study_subject_id=id), by = c("control"="name", "created", "last_updated", "study_id")) %>%
		  select(created,last_updated,control,study_subject_id,sheet_id,all_of(colnames(user_data)))

		## Find the locations and bags and join
		df.payload = df.payload %>%
		  inner_join(dbReadTable(con, "location") %>%
		               select(-c(created, last_updated)) %>%
		               dplyr::rename(location_id=id, location_name=name)
		             , by = c("location_name", "level_I", "level_II")) %>%
		  dplyr::left_join(dbReadTable(con, "dbs_bag") %>%
		                     select(-c(created, last_updated)) %>%
		                     dplyr::rename(dbs_bag_id=id), by=c("manifest_name"="name", "location_id")) %>%
		  filter(is.na(dbs_bag_id)) %>%
		  select(created,last_updated,control,study_subject_id,location_id,dbs_bag_id,sheet_id, all_of(colnames(user_data)))

		## if the bag does not exist (dbs_bag_id == `NA`), then create it
		res <- dbAppendTable(con, "dbs_bag", df.payload %>%
		                       filter(is.na(dbs_bag_id)) %>%
		                       select(created, last_updated, location_id, manifest_name) %>%
		                       dplyr::rename(name = manifest_name) %>%
		                       distinct())

		df.payload=dbReadTable(con, "dbs_bag") %>% dplyr::rename(bag_id=id) %>%
		  inner_join(df.payload %>% select(-c(created,last_updated)), by = c("location_id", "name"="manifest_name")) %>%
		  dplyr::rename(manifest_name=name) %>%
		  select(created,last_updated,control, study_subject_id,location_id,bag_id,sheet_id,all_of(colnames(user_data)))

		## Multiple sheets can go into a bag. Instead of identifying them uniquely, add a unique integer
		## for each control sheet in the background

		## Use a separate payload to subset sheet_ids
		df.payload.1=df.payload %>%
		  left_join(dbReadTable(con, "dbs_control_sheet")  %>% dplyr::rename(dbs_control_sheet_id=id), by=c("bag_id")) %>%
		  select(sheet_id,bag_id, uid) %>%
		  distinct() %>%
		  group_by(bag_id) %>%
		  dplyr::mutate(base.uid=ifelse(is.na(max(uid)), 0, max(uid))) %>%
		  group_by(bag_id, sheet_id) %>%
	    dplyr::mutate(sheet_id=as.integer(sheet_id), uid=sheet_id + base.uid) %>%
		  ungroup() %>%
		  distinct()

		## add the control sheet
		res <- dbAppendTable(con, "dbs_control_sheet", df.payload.1 %>% ungroup() %>% select(bag_id,uid))

		df.payload = df.payload.1 %>%
		  select(bag_id,uid,sheet_id) %>%
		  distinct() %>%
		  inner_join(df.payload, by=c("bag_id", "sheet_id"))

		## Read back in to get the dbs control sheet IDs
		df.payload=dbReadTable(con, "dbs_control_sheet") %>%
		  dplyr::rename(dbs_control_sheet_id=id) %>%
		  inner_join(df.payload, by = c("bag_id", "uid"))

		res <- dbAppendTable(con, "control", df.payload %>% dplyr::mutate(state_id=1, status_id=1) %>% select(density, state_id, status_id))

		df.payload = df.payload %>%
			mutate(
			  strain2 = strsplit(strain, ";"),
			  percentage2 = strsplit(percentage, ";")
			) %>%
			tidyr::unnest(cols = c("strain2","percentage2")) %>%
			left_join(dbReadTable(con, "strain") %>% dplyr::rename(strain_id = id, strain = name), by = c("strain2"="strain")) %>%
			left_join(dbReadTable(con, "control") %>% dplyr::rename(study_subject_id = id), by = c("study_subject_id", "density")) %>%
			rename(control_id = study_subject_id)

		res <- dbAppendTable(con, "control_strain", df.payload %>% select(control_id, strain_id, percentage2) %>% dplyr::rename(percentage=percentage2))

		res <- dbAppendTable(con, "dbs_control", df.payload %>% select(dbs_control_sheet_id, control_id))

		msg=sprintf("Successfully uploaded %s DBS Controls!", res)

		dbCommit(con)
		message(msg)
	},
	error = function(e) {
		message(e$message)

		# need to record error globally here
	},
	finally = {
		dbDisconnect(con)
	})
}

