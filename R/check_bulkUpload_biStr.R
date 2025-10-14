#' Check structure of Business Importance Input
#'
#' The online Tool expects that the categorical Business Importance is given as
#' "Low", "Medium", "High" or "Unknown". This function certifies if the entries
#' are provided using the required pattern, and flags eventual issues otherwise.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#'
#' @return Warnings flagging if 'Business Importance' labels diverge from expected
#' format.
#'
#' @examples
#' # if there are typos in the expected Business Importance labels:
#'  df1 <- data.frame("Company Name" = c("Company A", "Company A", "Company A"),
#'                    "Site Name" = c("Site 1", "Site 2", "Site 3"),
#'                    "Business Importance" = c("high", "very low", "Medium"),
#'                    check.names = FALSE)
#' check_bulkUpload_biStr(df1)
#'
#' # if everything is filled as expected
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_biStr(df2)

check_bulkUpload_biStr <- function(df) {

  allowed_values <- c("Low", "Medium", "High", "Unknown")

  # Loop through each row
  for (i in seq_len(nrow(df))) {
    site <- df$`Site Name`[i]
    bi   <- df$`Business Importance`[i]

    # Only check if not NA/empty
    if (!is.na(bi)) {
      if (!(bi %in% allowed_values)) {
        warning(paste0(
          "Row ", i, " (Site: ", site, "): Business Importance value '",
          bi, "' is invalid. Allowed values: ", paste(allowed_values, collapse = ", ")
        ))
      }
    }
    else {
      message("All entries for 'Business Importance' fulfill the expected structure.")
      return(invisible(NULL))
    }
  }
}
