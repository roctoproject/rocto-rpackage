context("Job running")

a <- capture.output({
  canRunFromFile <- tryCatch(roctoRun("../lib/5-toPack.rocto", "fileResult"),
                             error = function(e) e,
                             warning = function(w) w)
  canRunFromDir <- tryCatch(roctoRun("../lib/6-unPacked", "dirResult"),
                            error = function(e) e,
                            warning = function(w) w)
})

test_that("Rocto can run iterations from .rocto files and folders", {
  expect_true(canRunFromFile)
  expect_true(canRunFromDir)
})

tempenv <- new.env()
load(normalizePath(file.path("fileResult", list.files("fileResult"))), tempenv)
fileOutput <- tempenv$o
load(normalizePath(file.path("dirResult", list.files("dirResult"))), tempenv)
dirOutput <- tempenv$o

test_that("Results are as expected", {
  expect_equal(fileOutput, 75.69823)
  expect_equal(dirOutput, 75.69823)
})

# clean up files
unlink("fileResult", recursive = TRUE)
unlink("dirResult", recursive = TRUE)
unlink("../lib/5-toPack.rocto")
