# title: simulate malaria surveillance data
# objective: generate a tidy district-month dataset for trying the
#   malutils functions (calculate_tpr, calculate_incidence)
# author: mo yusuf
# date: 2026-06-10

# this script writes a derived csv to data-raw/; it never overwrites
# any raw source data. run it once, then load the csv to experiment.

set.seed(1051)

# reporting units and period ------------------------------------------

districts <- c(
  "kakamega", "kisumu", "siaya", "homa_bay",
  "migori", "busia", "vihiga", "bungoma"
)

reporting_months <- seq(
  from = as.Date("2024-01-01"),
  to = as.Date("2024-12-01"),
  by = "month"
)

# one row per district per month, in tidy long format -----------------

surveillance_2024 <- tidyr::expand_grid(
  district = districts,
  month = reporting_months
) |>
  dplyr::mutate(
    population = round(stats::runif(dplyr::n(), 40000, 160000)),
    tested = stats::rpois(dplyr::n(), lambda = 600),
    # seasonal positivity that peaks mid-year
    seasonal_risk = 0.18 +
      0.12 * sin(2 * pi * (lubridate::month(month) - 3) / 12),
    seasonal_risk = pmax(pmin(seasonal_risk, 0.95), 0.01),
    confirmed = stats::rbinom(
      n = dplyr::n(),
      size = tested,
      prob = seasonal_risk
    )
  ) |>
  dplyr::select(district, month, population, tested, confirmed) |>
  dplyr::arrange(district, month)

# persist a dated, derived copy ---------------------------------------

readr::write_csv(
  surveillance_2024,
  here::here("data-raw", "surveillance_2024.csv")
)

# optional: bake it into the package as an exported dataset
# usethis::use_data(surveillance_2024, overwrite = TRUE)
