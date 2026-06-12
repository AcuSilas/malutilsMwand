#' Calculate the malaria test positivity rate
#'
#' Computes the test positivity rate (TPR), the share of malaria tests
#' that return a positive result, expressed as a percentage. Records
#' with no tests return `NA` rather than a divide-by-zero result.
#'
#' @param confirmed Numeric vector of confirmed (positive) tests.
#' @param tested Numeric vector of people tested, the same length as
#'   `confirmed`.
#' @param digits Number of decimal places to round the result to.
#' @return Numeric vector of test positivity rates as percentages, the
#'   same length as the inputs.
#' @examples
#' calculate_tpr(c(45, 12, 7), c(120, 80, 0))
#' @export
calculate_tpr <- function(confirmed, tested, digits = 1) {
  stopifnot(
    is.numeric(confirmed),
    is.numeric(tested),
    length(confirmed) == length(tested)
  )

  positivity <- confirmed / tested * 100

  dplyr::if_else(
    tested > 0,
    round(positivity, digits),
    NA_real_
  )
}
