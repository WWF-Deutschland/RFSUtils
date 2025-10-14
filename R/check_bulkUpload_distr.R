#' Check distribution of input data
#'
#' As an exploratory check, this function calculates the distribution of input
#' data across different categories (group, industry, business importance).
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#' @param export default: FALSE. If TRUE, exports a .xlsx file with summary of
#' identified duplicate data
#' @param output_dir file path to the folder where Excel should be saved in case
#' export is set as TRUE. default: NULL
#'
#' @return Data frame summarizing the data distribution (count and percentage)
#' per category across key fields (Group, Industry, and Business Importance).
#'
#' @examples
#' df1 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_distr(df1, export = TRUE, output_dir = temp_dir())
#'

check_bulkUpload_distr <- function(df, export = FALSE, output_dir = NULL) {

  target_cols <- c("Industry", "Group (Optional)", "Business Importance")
  results <- list()

  for (col_name in target_cols) {
    col_data <- df[[col_name]]

    # Skip column if it's entirely NA (often the case for column 'Group')
    if (all(is.na(col_data))) {
      message(paste0("Skipping column '", col_name, "' (all NA)."))
      next
    }

    # Compute distribution
    distr <- df |>
      mutate(temp_col = ifelse(is.na(.data[[col_name]]),
                               "Not defined/NA",
                               as.character(.data[[col_name]]))) |>
      group_by(temp_col) |>
      summarise(
        Count = n(),
        Percentage = round(n() / nrow(df) * 100, 2),
        .groups = "drop"
      ) |>
      arrange(desc(Count))

    results[[col_name]] <- distr
  }

  # Export if requested
  if (export) {
    if (is.null(output_dir)) {
      stop("Please provide 'output_dir' when export = TRUE.")
    }

    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Package 'writexl' is required for export. Please install it.")
    }

    file_path <- file.path(output_dir, "distribution_summary.xlsx")

    writexl::write_xlsx(results, path = file_path)

    message("Excel file exported to: ", file_path)
  }

  return(results)
}
