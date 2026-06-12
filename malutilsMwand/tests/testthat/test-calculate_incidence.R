testthat::test_that("incidence scales cases to the denominator", {
  testthat::expect_equal(
    calculate_incidence(c(50, 10), c(10000, 5000)),
    c(5.00, 2.00)
  )
})

testthat::test_that("the per argument changes the scale", {
  testthat::expect_equal(
    calculate_incidence(50, 10000, per = 100000),
    500
  )
})

testthat::test_that("non-positive population returns NA", {
  testthat::expect_true(
    is.na(calculate_incidence(20, 0))
  )
})
