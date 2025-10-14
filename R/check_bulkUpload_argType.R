#' Check argument types in Bulk Uploads table
#'
#' The RFS Tool expects specific argument types for running the assessments.
#' This function certifies if data fulfills requirements and flags eventual issues.
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#'
#' @return Warnings flagging non-expected argument types.
#'
#' @examples
#' # if argument types do not match expected structure:
#' df1 <-
#'   data.frame("Company Name" = c(5, 9, 1),
#'              "Site Name" = c(8, 5, 7),
#'              check.names = FALSE)
#' check_bulkUpload_argType(df1)
#'
#' # if argument types fulfill requirements:
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_argType(df2)

check_bulkUpload_argType <- function(df) {
  if (!is.data.frame(df)) {
    stop("The input must be a dataframe.")
  }

  # Define expected column types
  expected_types <- c(
    "Company Name" = c("character"),
    "Site Name" = c("character", "integer"),
    "Industry" = c("character"),
    "Commodity (Optional)" = c("character"),
    "Group (Optional)" = c("character"),
    "Business Importance" = c("character"),
    "Latitude" = c("numeric"),
    "Longitude" = c("numeric"),
    "Address" = c("character"),
    # "...10" = c("NULL"),
    "O4a" = c("numeric"),
    "o25a5a" = c("numeric")
  )

  # store all issues in a df called 'violations'
  violations <- data.frame(
    column = character(),
    actual_class = character(),
    expected_class = character(),
    stringsAsFactors = FALSE
  )

  # Loop through expected columns
  for (col in names(expected_types)) {
    if (!col %in% names(df)) next  # Skip missing columns

    value <- df[[col]]

    # Skip empty columns
    if (all(is.na(value))) next

    actual_class <- class(value)[1]

    # Simplify type logic: treat integer as numeric
    if (actual_class == "integer") actual_class <- "numeric"

    if (!actual_class %in% expected_types[[col]]) {
      violations <- rbind(
        violations,
        data.frame(
          column = col,
          actual_class = actual_class,
          expected_class = paste(expected_types[[col]], collapse = ", "),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Summarize results
  if (nrow(violations) > 0) {
    warning("Some columns do not have the expected data types.")
    print(violations)
  } else {
    message("All non-empty fields contain expected data types.")
  }

  invisible(violations)
}

