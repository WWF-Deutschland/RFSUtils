#' Wrapper to run all 'check' functions for bulk uploads data
#'
#' Runs all check_bulkUpload_ functions sequentially and exports outputs if desired.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#' @param export default: FALSE. If TRUE, exports .xlsx files with results
#' @param output_dir file path to the folder where .xlsx files should be saved.
#' Only applies if export = TRUE. default: NULL
#'
#' @return Data frame summarizing the data distribution (count and percentage)
#' per category across key fields (Group, Industry, and Business Importance).
#'
#' @examples
#' df1 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_all(df1, export = TRUE, output_dir = output_dir)


check_bulkUpload_all <- function(df, export = FALSE, output_dir = NULL) {
  if (!is.data.frame(df)) stop("Input must be a dataframe.")

  message("Starting bulk upload validation...")

  results <- list()

  # 1. Check column structure
  message("Checking column structure...")
  results$argType <- tryCatch(
    check_bulkUpload_colStr(df),
    error = function(e) e
  )

  # 2. Check argument types
  message("Checking column data types...")
  results$argType <- tryCatch(
    check_bulkUpload_argType(df),
    error = function(e) e
  )

  # 3. Check missing data
  message("Checking for missing mandatory data...")
  results$missingData <- tryCatch(
    check_bulkUpload_missingData(df, export = export, output_dir = output_dir),
    error = function(e) e
  )

  # 4. Check industry sectors
  message("Checking industry sector classification")
  results$indStr <- tryCatch(
    check_bulkUpload_indStr(df),
    error = function(e) e
  )

  # 5. Check business importance
  message("Checking business importance inputs")
  results$indStr <- tryCatch(
    check_bulkUpload_biStr(df),
    error = function(e) e
  )

  # 6. Check duplicates
  message("Checking if there is duplicate data")
  results$indStr <- tryCatch(
    check_bulkUpload_dupl(df, export = export, output_dir = output_dir),
    error = function(e) e
  )

  # 7. Check data distribution
  message("Checking data distribution across groups, industries, and
          business importance categories...")
  results$distrInput <- tryCatch(
    check_bulkUpload_distr(df, export = export, output_dir = output_dir),
    error = function(e) e
  )

  message("All checks completed.")

  return(results)
}
