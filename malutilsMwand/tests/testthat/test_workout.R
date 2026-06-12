testthat::test_that("A known workout returns the right points and tier", {
  result <- workout_reward(8, c(pushups = 20, squats = 30, burpees = 10),
    announce = FALSE)
  testthat::expect_equal(result$per_round, 70)
  testthat::expect_equal(result$points, 560)
  testthat::expect_equal(result$tier, "Gold")
})

testthat::test_that("A bad input throws a clear error", {
  testthat::expect_error(workout_reward(-3, announce = FALSE), "cannot be negative")
  testthat::expect_error(workout_reward(c(1, 2), announce = FALSE), "single number")
  testthat::expect_error(workout_reward(5, c(1, 2), announce = FALSE), "`difficulty` must be a named numeric vector")
})

testthat::test_that("more rounds and harder combos score higher", {
  testthat::expect_gt(workout_reward(10, announce = FALSE)$points,
    workout_reward(5,  announce = FALSE)$points)
  testthat::expect_gt(workout_reward(1, c(pullups = 10), announce = FALSE)$per_round,
    workout_reward(1, c(pushups = 10), announce = FALSE)$per_round)
})