context("Job checking")
a <- capture.output({
  wrongParams <- tryCatch(roctoCheck("tests/lib/1-wrongParams"), 
                          error = function(e) e,
                          warning = function(w) w)
  wrongMain <- tryCatch(roctoCheck("tests/lib/2-wrongMain"), 
                        error = function(e) e,
                        warning = function(w) w)
  noSource <- tryCatch(roctoCheck("tests/lib/3-noSource"), 
                       error = function(e) e,
                       warning = function(w) w)
  noData <- tryCatch(roctoCheck("tests/lib/4-noData"), 
                     error = function(e) e,
                     warning = function(w) w)
})


test_that("Parameter mismatch is detected", {
  expect_true(inherits(wrongParams, "simpleWarning"), 
              info = "Params file param mismatch not detected")
  expect_true(inherits(wrongMain, "simpleWarning"), 
              info = "Main file param mismatch not detected")
})


test_that("Missing used files are detected", {
  expect_true(inherits(noSource, "simpleWarning"), 
              info = "Lack of source file not detected")
  expect_true(inherits(noData, "simpleWarning"), 
              info = "Lack of data file not detected")
})

