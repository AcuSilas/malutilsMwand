testthat::test_that("tpr is positives over tested as a percentage", {
  testthat::expect_equal(
    calculate_tpr(c(45, 12), c(120, 80)),
    c(37.5, 15.0)
  )
})

testthat::test_that("zero tested returns NA, not an error", {
  testthat::expect_true(
    is.na(calculate_tpr(7, 0))
  )
})

testthat::test_that("mismatched lengths are rejected", {
  testthat::expect_error(
    calculate_tpr(c(1, 2), 10)
  )
})
