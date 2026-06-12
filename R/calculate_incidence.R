#' Calculate malaria incidence per population
#'
#' Computes malaria incidence as confirmed cases scaled to a population
#' denominator, for example cases per 1000 people. Records with a
#' non-positive population return `NA` rather than a divide-by-zero
#' result.
#'
#' @param cases Numeric vector of confirmed cases.
#' @param population Numeric vector of population at risk, the same
#'   length as `cases`.
#' @param per Denominator scale for the rate, for example `1000` for
#'   cases per 1000 population.
#' @param digits Number of decimal places to round the result to.
#' @return Numeric vector of incidence rates per `per` population, the
#'   same length as the inputs.
#' @examples
#' calculate_incidence(c(45, 12), c(8200, 5400))
#' calculate_incidence(c(45, 12), c(8200, 5400), per = 100000)
#' @export
calculate_incidence <- function(cases,
                                population,
                                per = 1000,
                                digits = 2) {
  stopifnot(
    is.numeric(cases),
    is.numeric(population),
    length(cases) == length(population)
  )

  rate <- cases / population * per

  dplyr::if_else(
    population > 0,
    round(rate, digits),
    NA_real_
  )
}
