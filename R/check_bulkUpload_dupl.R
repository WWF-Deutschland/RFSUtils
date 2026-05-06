#' Check duplicates in bulk input data
#'
#' As a basic quality check on the data to be used for the analysis, it should
#' be assured that no duplicates exist on key fields. This function certifies if
#' the following types of duplicates occur:
#'   - full duplicate entry (all fields are identical)
#'   - location duplicates (either for both Latitude & Longitude, or Addresses)
#'   - repeated identifier (identical Site Name)
#'
#' @param df Dataframe. Refers to the sheet "Add your sites here" of the Excel
#' template for bulk upload of sites.
#' @param export default: FALSE. If TRUE, exports a .xlsx file with summary of
#' identified duplicate data
#' @param output_dir file path to the folder where Excel should be saved in case
#' export is set as TRUE. default: NULL
#'
#' @return List with extract of duplicates. 'full_list' an extract of all entries
#' that contain some kind of duplicate data. 'dupl_class' outlines which case of
#' duplicate occurs for each extracted entry. 'summary' provides a count of how
#' how many cases occur for each type of duplicate value.
#'
#' @examples
#' # if there are duplicates in the input data:
#'  df1 <- data.frame("Company Name" = c("Company A", "Company A", "Company A", "Company A"),
#'                    "Site Name" = c("Site 1", "Site 2", "Site 1", "Site 2"),
#'                    "Industry" = c("Agriculture", "Retail", "Textiles", "Retail"),
#'                    "Commodity (Optional)" = c(NA, NA, NA, NA),
#'                    "Group (Optional)" = c(NA, NA, NA, NA),
#'                    "Business Importance" = c("Low", "High", "Medium", "High"),
#'                    "Latitude" = c(51.7, 53.4, 51.7, 53.4),
#'                    "Longitude" = c(30.9, 21.8, 30.9, 21.8),
#'                    "Address" = c(NA, NA, NA, NA),
#'                    "O4a" = c(NA, NA, NA, NA),
#'                    "O25a"= c(NA, NA, NA, NA),
#'                    check.names = FALSE
#'                    )
#' check_bulkUpload_dupl(df1, export = TRUE, output_dir = tempdir())
#'
#' # if everything is filled as expected
#' df2 <- readxl::read_excel(system.file("extdata",
#'                                       "test_sites_bulkupload_windustry.xlsx",
#'                                        package = "RFSUtils"), sheet = "Add your sites here")
#' check_bulkUpload_dupl(df2)


check_bulkUpload_dupl <- function(df, export = FALSE, output_dir = NULL) {
  violations <- data.frame(
    Violation = character(),
    Rows = character(),
    Sites = character(),
    stringsAsFactors = FALSE
  )

  problem_rows <- data.frame()

  add_violation_group <- function(dup_groups, violation_type, detail_cols = NULL) {
    for (grp in dup_groups) {
      if (length(grp) > 1) {
        sites <- if ("Site Name" %in% names(df)) as.character(df[grp, "Site Name"]) else rep("", length(grp))

        # Add to summary table
        violations <<- rbind(
          violations,
          data.frame(
            Violation = violation_type,
            Rows = paste(grp, collapse = ", "),
            Sites = paste(sites, collapse = ", "),
            stringsAsFactors = FALSE
          )
        )

        # Add to problem rows table
        temp <- df[grp, , drop = FALSE]
        temp$Violation <- violation_type
        problem_rows <<- rbind(problem_rows, temp)

        warning(paste0(
          violation_type, " — rows ", paste(grp, collapse = ", "),
          " (Sites: ", paste(sites, collapse = ", "), ")"
        ))
      }
    }
  }

  # 1. Fully duplicated rows
  dup_full <- duplicated(df) | duplicated(df, fromLast = TRUE)
  dup_full_groups <- split(which(dup_full | duplicated(df)),
                           lapply(df[dup_full | duplicated(df), , drop = FALSE], paste, collapse = "_"))
  dup_full_groups <- Filter(function(g) length(g) > 1, dup_full_groups)
  add_violation_group(dup_full_groups, "Fully duplicate row")

  rows_to_skip <- unique(unlist(dup_full_groups))

  # 2. Duplicate coordinates (both lat & lon must match, not both NA)
  if (all(c("Latitude", "Longitude") %in% names(df))) {
    coord_key <- paste(df$Latitude, df$Longitude)
    # Exclude rows where both coordinates are NA
    both_na <- is.na(df$Latitude) & is.na(df$Longitude)
    dup_coords <- (duplicated(coord_key) | duplicated(coord_key, fromLast = TRUE)) & !both_na
    dup_coord_groups <- split(which(dup_coords), coord_key[dup_coords])
    dup_coord_groups <- lapply(dup_coord_groups, setdiff, rows_to_skip)
    dup_coord_groups <- Filter(function(g) length(g) > 1, dup_coord_groups)
    add_violation_group(dup_coord_groups, "Duplicate coordinates")
  }

  # 3. Duplicate address (skip if address is NA in both)
  if ("Address" %in% names(df)) {
    addr_key <- df$Address
    both_na_addr <- is.na(addr_key)

    dup_addr <- (duplicated(addr_key) | duplicated(addr_key, fromLast = TRUE)) & !both_na_addr

    dup_addr_groups <- split(which(dup_addr), addr_key[dup_addr])
    dup_addr_groups <- lapply(dup_addr_groups, setdiff, rows_to_skip)
    dup_addr_groups <- Filter(function(g) length(g) > 1, dup_addr_groups)
    add_violation_group(dup_addr_groups, "Duplicate address")
  }

  # 4. Duplicate site names
  if ("Site Name" %in% names(df)) {
    dup_sites <- duplicated(df$`Site Name`) | duplicated(df$`Site Name`, fromLast = TRUE)
    dup_site_groups <- split(which(dup_sites), df$`Site Name`[dup_sites])
    dup_site_groups <- lapply(dup_site_groups, setdiff, rows_to_skip)
    dup_site_groups <- Filter(function(g) length(g) > 1, dup_site_groups)
    add_violation_group(dup_site_groups, "Duplicate site name")
  }

  if (nrow(violations) > 0) {

  # Summary table
  summary_table <- as.data.frame(table(violations$Violation))
  names(summary_table) <- c("Violation", "Count")

  message("Duplicate check complete.")

  results <- list(
    full_list = violations,
    dupl_class = problem_rows,
    summary = summary_table)

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
      path = file.path(output_dir, "duplicate_data_summary.xlsx")
    )

    message("Excel file exported to: ", file.path(output_dir, "duplicate_data_summary.xlsx"))
  }

  return(invisible(results))

  } else {
    message("There are no duplicates in the provided data.")
    return(invisible(NULL))
  }

}
