% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_num_invGeometricMean.R
\name{calc_num_invGeometricMean}
\alias{calc_num_invGeometricMean}
\title{Calculate aggregate risk using inverse geometric mean}
\usage{
calc_num_invGeometricMean(input, na.rm = TRUE, nodata.rm = TRUE)
}
\arguments{
\item{input}{Vector of risk scores (values 1-5)}

\item{na.rm}{Logical. Should NA values be ignored? TRUE by default.
NOTE: Will still return NA if there are no non-NA data.}

\item{nodata.rm}{Logical. Should no data (0) values be ignored? TRUE by default.
NOTE: Will return NA if there are no no data data.}
}
\value{
Numeric between 1-5.
}
\description{
Calculate aggregate risk using inverse geometric mean
}
\examples{
## Calculate for a single vector
calc_num_invGeometricMean(c(1, 5, 4, 2))

## Use within a dataframe (column)
    data.frame(group = c("A", "A", "B", "B", "B"),
               risk_score = c(1, 4, 3, 2, 5)) |>
        dplyr::group_by(group) |>
        dplyr::summarise(invGeom = calc_num_invGeometricMean(risk_score))

## Use within a dataframe (row)
## NOTE: Unlike S&P approach, higher risk values get higher weighting
if(require("lay", quietly = TRUE)){
    data.frame(hydroshed = c(1, 2, 3),
               risk_score1 = c(1, 2, 5),
               risk_score2 = c(2, 2, 3),
               risk_score3 = c(5, 4, 1)) |>
        dplyr::mutate(invGeom = lay(pick(-hydroshed), calc_num_invGeometricMean))
}
}
