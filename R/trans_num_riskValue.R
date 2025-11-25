#' Convert numeric data into 1-5 risk values.
#'
#' Intended for use within pipes and can be applied across multiple columns.
#' See example.
#'
#' @param input Numeric vector. Original state of nature data to convert.
#' @param breaks Numeric vector length 6. In general, it is good to use -Inf and Inf
#' as the minimum/maximum values to avoid issues when comparing decimals.
#' @param fn List of functions (length 5) to create breaks.
#' By default, breaks are applied as: <=, <=, <=, <=, >
#' @param direction Integer. Do low values in input correspond to
#' low risk (direction = 1) or high risk (direction = -1)?
#'
#' @returns A numeric vector
#' @export
#'
#' @examples
#' ## Apply breaks to iris data as an example
#' my_breaks <- c(-Inf, 4.5, 5.5, 6.5, 7.5, Inf)
#'
#' ## Apply to one column using dplyr & pipe
#' iris |>
#'   mutate(riskValue = trans_num_riskValue(Sepal.Length, my_breaks))
#'
#' ## Apply to multiple columns using across
#' iris |>
#'   mutate(across(.cols = c(Sepal.Length, Sepal.Width),
#'                 \(x) trans_num_riskValue(x, breaks = my_breaks,
#'                                          direction = -1)))
trans_num_riskValue <- function(input, breaks, fn, direction = 1){

  ## Check input is numeric
  if (!is.numeric(input)){
    stop("'input' should be numeric")
  }

  ## Check that breaks are correct
  if (!is.numeric(breaks)){
    stop("'breaks' should be numeric")
  }
  if (length(breaks) != 6){
    stop("'breaks' should be length 6")
  }

  if (!missing(fn)){
    if (length(fn) != 5){
      stop("Need to provide 5 functions")
    }
  } else {
    fn <- list(`<=`, `<=`, `<=`, `<=`, `>`)
  }

  if (direction == 1){
    risk_vec <- 1:5
  } else if (direction == -1){
    risk_vec <- 5:1
  } else {
    stop("'direction' should be 1 or -1")
  }

  output <- case_when(
    ## Missing data passed forward if present
    input == -999 ~ -999,
    input == -666 ~ -666,
    ## Because we do this in order, it's not
    ## necessary for us to do breaks[2] < x <= breaks[3]
    ## Everything for risk value 1 has already been assigned
    fn[[1]](input, breaks[2]) ~ risk_vec[1],
    fn[[2]](input, breaks[3]) ~ risk_vec[2],
    fn[[3]](input, breaks[4]) ~ risk_vec[3],
    fn[[4]](input, breaks[5]) ~ risk_vec[4],
    fn[[5]](input, breaks[5]) ~ risk_vec[5],
  )

  return(output)

}
