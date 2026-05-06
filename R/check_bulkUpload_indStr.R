#' Check structure of Industry sector Input
#'
#' The online Tool expects that the industry labels for the 25 sectors within the
#' RFS are provided using standard namings. This function certifies if the entries
#' are provided using the required pattern, and flags eventual issues otherwise.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#'
#' @return Warnings flagging if 'Industry' labels diverge from expected
#' format.
#'
#' @examples
#' # if there are typos in the expected Industry labels:
#'  df1 <- data.frame("Company Name" = c("Company A", "Company A", "Company A"),
#'                    "Site Name" = c("Site 1", "Site 2", "Site 3"),
#'                    "Industry" = c("agri", "textiles", "Construction Materials"),
#'                    check.names = FALSE)
#' check_bulkUpload_indStr(df1)
#'
#' # if everything is filled as expected
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_indStr(df2)


check_bulkUpload_indStr <- function(df) {

  industry_table <- read.csv(system.file("extdata","constants",
                                         "RFS_industry_translation.csv",
                                         package = "RFSUtils"))

  allowed_industries <- industry_table$Industry

  # Data frame to store invalid entries
  invalid_entries <- data.frame(
    Row = integer(),
    Site = character(),
    Industry = character(),
    stringsAsFactors = FALSE)

  # Loop through each row
  for (i in seq_len(nrow(df))) {
    site <- df$`Site Name`[i]
    ind   <- df$`Industry`[i]

    if (!is.na(ind)) {
      if (!(ind %in% allowed_industries)) {
        invalid_entries <- rbind(
          invalid_entries,
          data.frame(Row = i, Site = site, Industry = ind, stringsAsFactors = FALSE)
        )
      }
    }
  }

  # Handle results
  if (nrow(invalid_entries) > 0) {
    warning("Some Industry entries are invalid.")
    print(invalid_entries)
    cat("\n Allowed Industry values are:\n",
        paste(allowed_industries, collapse = ", "), "\n")
  } else {
    message("All entries for 'Industry' fulfill the expected structure.")
  }

  invisible(invalid_entries)
}
