#' Check column structure in Bulk Uploads table
#'
#' The online Tool expects a precise column structure for data uploads as provided
#' on the Excel template. This function flags if the user has made any unexpected
#' custom changes.The Excel should contain all pre-specified fields without the
#' addition of new columns.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#'
#' @return Warnings flagging missing or additional columns.
#'
#' @examples
#' # if columns do not match expected structure:
#' df1 <-
#'   data.frame(A = c(5, 9, 1),
#'              B = c(8, 5, 7),
#'              check.names = FALSE)
#' check_bulkUpload_colStr(df1)
#'
#' # if column structure fulfills requirements:
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_colStr(df2)
#'

check_bulkUpload_colStr <- function(df) {
  # Ensure df is actually a dataframe
  if (!is.data.frame(df)) {
    stop("The input must be a dataframe.")
  }

  # Define expected columns
  expected_cols <- c("Company Name","Site Name","Industry","Commodity (Optional)", "Group (Optional)","Business Importance","Latitude","Longitude","Address","...10","O4a","O25a")

  # Identify missing and extra columns
  missing_cols <- setdiff(expected_cols, names(df))
  extra_cols   <- setdiff(names(df), expected_cols)

  # Warnings
  if (length(missing_cols) > 0) {
    warning(paste("The dataframe lacks expected columns:", paste(missing_cols, collapse = ", "), ". Plese keep the original structure as given on the template."))
  }
  if (length(extra_cols) > 0) {
    warning(paste("The dataframe has additional columns:", paste(extra_cols, collapse = ", "), ". Plese keep the original structure as given on the template."))
  }
  if (length(missing_cols) == 0 && length(extra_cols) == 0) {
    message(paste0("All column names in '", deparse(substitute(df)), "' are as expected."))
  }
}
