context("Loading results into R")

out <- roctoResults("../lib/7-testResults.rocres")

test_that("Results are loaded in environment", {
  expect_true(is.list(out))
})

test_that("Results object contains appropriate content", {
  expect_equal(length(out),10)
  expect_equal(out[[1]], 75.69823)
})