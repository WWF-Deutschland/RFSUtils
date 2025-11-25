#' Invert risk score data
#'
#' A wrapper around [RFSUtils::trans_num_invert()] specifically for 1-5 risk score data.
#' The function will invert all risk scores, except those cases of '0 - No Data'
#' that will remain unchanged.
#'
#' @param risk_score An ordered factor or numeric vector.
#' For an ordered factor, should be an object of class ordered factor that has
#' levels specified in the object risk_scores. For numeric vector, expects
#' values between 0 and 5.
#' @param return_fct Logical. Should data be returned as a factor with the same
#' levels as the input or as a numeric (default).
#' @param ... Placeholder for other arguments. Currently ignored.
#' @param tol_val Numeric. Tolerance when comparing decimal values.
#' Important when checking aggregated data (decimal risk values) to
#' see if they are between 1-5. Default: 1e-5.
#'
#' @return Return inverted values of the risk score, either as a numeric vector
#' or ordered factor, depending on the value of `return_fct`
#' @export
#'
#' @examples
#' ## Create a risk score input
#' ## NOTE: We use a 0 index below to more directly match our risk score levels
#' ## i.e. 0 = no data.
#' input_data <- factor(risk_scores[c(0, 5, 1) + 1],
#'                      levels = risk_scores,
#'                      ordered = TRUE)
#' input_data
#'
#' ## Invert all values and return numeric
#' ## NOTE: 0 remains unchanged!!
#' trans_riskscore_invert(input_data) ## 5 becomes 1 and 1 becomes 5
#'
#' ## Invert when we don't cover the full range
#' ## NOTE: Inversion still correct even though we don't have the full range
#' trans_riskscore_invert(input_data[1:2])
#'
#' ## Return ordered factor instead of numeric
#' trans_riskscore_invert(input_data[1:2], return_fct = TRUE)
#'
#' ## Example with non-integer values (e.g. geometric mean of two risk values)
#' trans_riskscore_invert(c(1.25, 0, 4.39))
trans_riskscore_invert <- function(risk_score, return_fct = FALSE, tol_val = 1e-5, ...){

  ## Previously 'trans_riskscore_invert'
  ## If data are ordered factor, convert to integers
  if (inherits(risk_score, "ordered")) {

    if (!all(c("0 - No data", "1 - Very low risk", "2 - Low risk", "3 - Moderate risk",
               "4 - High risk", "5 - Very high risk") %in% levels(risk_score))) {
      stop("Expects an ordered factor with levels from 0-5.")
    }

    ## First, convert out risk score to be integer
    ## Convert index so that no data always is 0
    risk_score_int <- as.integer(risk_score) - 1

  } else if (inherits(risk_score, c("numeric", "integer"))) {

    if ((0 - min(risk_score, na.rm = TRUE)) > tol_val | (max(risk_score, na.rm = TRUE) - 5) > tol_val) {
      stop("Expects a numeric or integer vector with all values between 0 and 5.")
    }

    risk_score_int <- risk_score

  } else {

    stop("'risk_score' should be either an ordered factor or numeric")

  }

  ## For all values that are NOT 0 (0 = No data, therefore doesn't change)
  ## Invert them with the knowledge that the range_sum should ALWAYS we 6
  ## i.e. 1 should equal 5, no matter the available data
  output <- risk_score_int
  ## Only invert data that is 1-5 (no 0 or NA)
  output[output != 0 & !is.na(output)] <- trans_num_invert(output[output != 0 & !is.na(output)],
                                                           range_sum = 6, ...)

  if (return_fct) {

    output <- factor(RFSTools::risk_scores[output + 1],
                     levels = c("0 - No data", "1 - Very low risk", "2 - Low risk", "3 - Moderate risk",
                                "4 - High risk", "5 - Very high risk"),
                     ordered = TRUE)

  }

  return(output)

}
