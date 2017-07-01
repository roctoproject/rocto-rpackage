context("Job skeleton creation")

expect_file_exists <- function(file) {
  expect_true(file.exists(file), 
              info = sprintf("File %s does not exist!", file))
}

expect_dir_exists <- function(dir) {
  expect_true(dir.exists(dir), 
              info = sprintf("Directory %s does not exist!", dir))
}


test_that("Correct files are created", {
  success <- roctoNew(interactive = FALSE)
  expect_true(base, info = "roctoNew function did not return true")
  expect_dir_exists("roctoJob")
  expect_file_exists("roctoJob/main.R")
  expect_file_exists("roctoJob/params.R")
  expect_dir_exists("roctoJob/data")
  expect_file_exists("roctoJob/data/helper.R")
  expect_file_exists("roctoJob/data/myData.csv")
})

unlink("roctoJob", recursive = TRUE, force = TRUE)
