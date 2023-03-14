#' Micronix NA
#'
#' An example upload file with a single Micronix sample using the NA file format
#'
#' @format ## `micronix_na`
#' A data frame with 1 rows and 13 columns:
#' \describe{
#'   \item{Barcode}{A 10 digit unique identifier for the sample}
#'   \item{Row}{A character denoting the row position of the sample}
#'   \item{Column}{An integer denoting the column position of the sample}
#' 	 \item{StudyCode}{The code associated with the study the sample is part of}
#'   \item{StudySubject}{The identifier of the study subject from which the sample was collected}
#'   \item{SpecimenType}{The specimen contained in the sample}
#' 	 \item{PlateName}{The name of the plate}
#'   \item{FreezerName}{The name of the freezer the sample will be in}
#'   \item{ShelfName}{The shelf the sample will be located in}
#'   \item{BasketName}{The place on the shelf where the sample will be located}
#'   \item{CollectionDate}{The date the sample was collected. This field is required if the sample is part of a `longitudinal study`, and MUST be in `YYYY-MM-DD` format}
#'   \item{Comment}{An optional field to leave a comment for the sample}
#'   \item{PlateBarcode}{An optional field to provide the barcode of the container the sample is located in}
#'   ...
#' }
#' @source EPPIcenter
"micronix_na"

#' Cryovial NA
#'
#' An example upload file with a single Cryovial sample using the NA file format
#'
#' @format ## `micronix_na`
#' A data frame with 1 rows and 13 columns:
#' \describe{
#'   \item{Barcode}{A 10 digit unique identifier for the sample}
#'   \item{BoxRow}{A character denoting the row position of the sample}
#'   \item{BoxColumn}{An integer denoting the column position of the sample}
#'   \item{SpecimenType}{The specimen contained in the sample}
#'   \item{StudySubject}{The identifier of the study subject from which the sample was collected}
#'   \item{CollectionDate}{The date the sample was collected. This field is required if the sample is part of a `longitudinal study`, and MUST be in `YYYY-MM-DD` format}
#' 	 \item{BoxName}{The name of the plate}
#'   \item{FreezerName}{The name of the freezer the sample will be in}
#'   \item{RackNumber}{The shelf the sample will be located in}
#'   \item{RackPosition}{The place on the shelf where the sample will be located}
#' 	 \item{StudyCode}{The code associated with the study the sample is part of}
#'   \item{Comment}{An optional field to leave a comment for the sample}
#'   \item{BoxBarcode}{An optional field to provide the barcode of the container the sample is located in}
#'   ...
#' }
#' @source EPPIcenter
"cryovial_na"