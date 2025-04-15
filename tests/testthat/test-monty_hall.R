testthat::test_that("monty_hall_sim works with 3 doors", {
  set.seed(123)
  result <- monty_hall_sim(3, 1000)
  testthat::expect_length(result, 2)
  testthat::expect_true(result$stick > 0.28 && result$stick < 0.38)
  testthat::expect_true(result$switch > 0.62 && result$switch < 0.72)
  testthat::expect_equal(result$stick + result$switch, 1, tolerance = 0.01)
})

testthat::test_that("monty_hall_sim works with 5 doors", {
  set.seed(123)
  result <- monty_hall_sim(5, 1000)
  testthat::expect_length(result, 2)
  testthat::expect_true(result$stick > 0.15 && result$stick < 0.25)
  testthat::expect_true(result$switch > 0.75 && result$switch < 0.85)
  testthat::expect_equal(result$stick + result$switch, 1, tolerance = 0.01)
})

testthat::test_that("monty_hall_sim errors with < 3 doors", {
  testthat::expect_error(monty_hall_sim(2, 100), "n_doors must be >= 3")
})
