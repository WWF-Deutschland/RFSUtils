#' Check missing data on mandatory fields in Bulk Uploads table
#'
#' Although some of the fields provided on the template are optional, users must
#' provide a mandatory set of information for all entries:
#' - basic identifiers: "Company Name","Site Name","Industry"
#' - location data (either as coordinates or addresses)
#' - business importance (either categorical or numerical)
#' This function certifies if data fulfills requirements and flags eventual issues.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#' @param export default: FALSE. If TRUE, exports a .xlsx file with summary of
#' identified missing data
#' @param output_dir file path to the folder where Excel should be saved in case
#' export is set as TRUE. default: NULL
#'
#' @return List with extract of violations. 'violations_full' contains all entries
#' that do not meet expectations and 'violations_summary' summarises how many cases
#' occur per violation type.
#'
#' @examples
#' # if mandatory data is missing:
#'  df1 <- data.frame("Company Name" = c("Company A", "Company A", NA),
#'                    "Site Name" = c("Site 1", NA, "Site 3"),
#'                    "Industry " = c("Agriculture", NA, "Textiles"),
#'                    "Commodity (Optional)" = c(NA, NA, NA),
#'                    "Group (Optional)" = c(NA, NA, NA),
#'                    "Business Importance" = c("High", "Low", NA),
#'                    "Latitude" = c(51.7, 53.4, NA),
#'                    "Longitude" = c(30.9, 21.8, -74.8),
#'                    "Address" = c(NA, NA, NA),
#'                    "O4a" = c(NA, NA, NA),
#'                    "O25a"= c(NA, NA, NA),
#'                    check.names = FALSE
#'                    )
#'  check_bulkUpload_missingData(df1, export = TRUE, output_dir = tempdir())
#'
#' # if everything is filled as expected
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_missingData(df2)



check_bulkUpload_missingData <- function(df, export = FALSE, output_dir = NULL) {

  # Dataframe to store all violations
  violations <- data.frame(
    Row = integer(),
    Site = character(),
    Violation = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(df))) {
    comp     <- df$`Company Name`[i]
    site     <- df$`Site Name`[i]
    lat      <- df$Latitude[i]
    lon      <- df$Longitude[i]
    addr     <- df$Address[i]
    bi       <- df$`Business Importance`[i]
    o25a     <- df$O25a[i]
    industry <- df$Industry[i]

    # Helper to add violation to dataframe
    add_violation <- function(msg) {
      violations <<- rbind(
        violations,
        data.frame(Row = i, Site = site, Violation = msg, stringsAsFactors = FALSE)
      )
      warning(paste0("Row ", i, " (Site: ", site, "): ", msg))
    }

    # Violation 1: no location data provided
    if (is.na(lat) & is.na(lon) & is.na(addr)) {
      add_violation("Missing location data: neither coordinates nor address provided")
    }

    # Violation 2: no address provided, and incomplete coordinates (missing lat or long)
    if ((!is.na(lat) & is.na(lon) & is.na(addr)) || (is.na(lat) & !is.na(lon) & is.na(addr))) {
      add_violation("Location data incomplete: check Latitude/Longitude inputs")
    }

    # Violation 3: no business importance provided
    if (is.na(bi) & is.na(o25a)) {
      add_violation("Business Importance info missing: either categorical or numeric (O25a) must be provided")
    }

    # Violation 4: no Company Name provided
    if (is.na(comp)) {
      add_violation("Company Name is missing")
    }

    # Violation 5: no Site Name provided
    if (is.na(site)) {
      add_violation("Site Name is missing")
    }

    # Violation 6: no Industry provided
    if (is.na(industry)) {
      add_violation("Industry is missing")
    }
  }

  if (nrow(violations) > 0) {

    # Summary table
    summary_table <- aggregate(Row ~ Violation, data = violations, FUN = length)
    names(summary_table)[2] <- "Count"

    results <- list(
      violations_full = violations,
      violations_summary = summary_table
    )

    # Optional export
    if (export) {
      if (is.null(output_dir)) {
        stop("Please provide 'output_dir' when export = TRUE.")
      }

      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' is required for export. Please install it.")
      }

      writexl::write_xlsx(
        results,
        path = file.path(output_dir, "missing_data_summary.xlsx")
      )

      message("Excel file exported to: ", file.path(output_dir, "missing_data_summary.xlsx"))
    }

    return(invisible(results))

  } else {
    message("All entries are provided with mandatory data inputs.")
    return(invisible(NULL))
  }

}
