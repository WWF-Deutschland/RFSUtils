#' Check percentage of missing data
#'
#' As an exploratory check, this function calculates the percentage of NAs across
#' the fields of the bulk uploads table.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#'
#' @return Data frame summarizing the percentage of per field.
#'
#' @examples
#' df1 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_NApct(df1)
#'

check_bulkUpload_NApct <- function(df) {

  na_pct <- sapply(df, function(col) {
    round(sum(is.na(col)) / length(col) * 100, 2)
  })

  na_table <- data.frame(
    Column = names(na_pct),
    NA_Percentage = na_pct
  )

  # Print a message
  message("NA percentage per column:")
  print(na_table)

  # invisible(na_table)  # returns the table invisibly
}
