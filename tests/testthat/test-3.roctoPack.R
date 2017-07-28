context("Job testing and packing")

expect_file_exists <- function(file) {
  expect_true(file.exists(file), 
              info = sprintf("File %s does not exist!", file))
}


a <- capture.output({
  toPack <- tryCatch(roctoPack("../lib/5-toPack", verbose = FALSE), 
                          error = function(e) e,
                          warning = function(w) w)
})


fullPath <- normalizePath("../lib/5-toPack.rocto")
test_that("A rocto file is created", {
  expect_file_exists(fullPath)
})

dirPath <- NULL
tdir <- tempdir()

test_that("Rocto file can be unzipped", {
  # unzip the file in order to check it
  uz <- try(utils::unzip(fullPath, exdir = tdir))
  if (inherits(uz, "try-error")) {
    stop("Unzip failed")
  }
  dirPath <<- file.path(tdir, "5-toPack")
  expect_false(inherits(uz, "try-error"))
})

test_that("Rocto folder contains all necessary files", {
  expect_file_exists(file.path(dirPath, "data/helper.R"))
  expect_file_exists(file.path(dirPath, "data/myData.csv"))
  expect_file_exists(file.path(dirPath, "grid.json"))
  expect_file_exists(file.path(dirPath, "main.R"))
  expect_file_exists(file.path(dirPath, "meta.json"))
  expect_file_exists(file.path(dirPath, "params.R"))
})

test_that("Created grid is valid", {
  gr <- jsonlite::fromJSON(readLines(file.path(dirPath, "grid.json")))
  expect_true(all(dim(gr) == c(9,2)))
  expect_true(all(colnames(gr) == c("param1", "param2")))
})

