# Evaluate a task

#' Evaluate one iteration from a rocto job.
#' 
#' This function runs one rocto job iteration - a "task".
#' 
#' @param roctoJob <character> Path to a \code{.rocto} file containing the job.
#' @param outputDir <character> Full directory where the output file should be saved.
#' @param iterId <integer> or <character: "test"> Indicates which task \code{[1:nIter]}
#' should be run, or whether the test parameters should be used.
#' 
#' @return Invisible boolean TRUE.
#' 
#' @seealso \code{\link{newJob}}, \code{\link{packJob}}, \code{\link{resultsToList}}
#'  
#' @export
evaluateTask <- function(roctoJob, outputDir, iterId = "test"){
  filebase <- strsplit(basename(roctoJob), "\\.")[[1]]
  if (tolower(filebase[2]) != "rocto") {
    stop("Supply proper roctoJob file")
  } 
  
  outputPath <- suppressWarnings(normalizePath(outputDir)) # TODO gracefully catch "dir does not exist"
  if (!dir.exists(outputPath)) {
    dir.create(outputPath, recursive = TRUE)
  }
  
  fullpath <- normalizePath(roctoJob)
  tdir <- tempdir()
  if (dir.exists(file.path(tdir, filebase[1]))) {
    unlink(file.path(tdir, filebase[1]), recursive = TRUE)
  }
  
  # unzip the file in order to use it
  uz <- try(utils::unzip(fullpath, exdir = tdir))
  if (inherits(uz, "try-error")) {
    stop("Unzip failed")
  }
  
  oldwd <- getwd()
  setwd(file.path(tdir, filebase[1]))
  
  if (iterId == "test"){
    source("params.R")
    p <- testParams
  } else {
    load("grid.Rdata")
    p <- as.list(grid[iterId,])
  }
  
  source("main.R")
  
  # convert parameters to correct order
  pSorted <- lapply(names(formals(main)), function(n) { p[[n]] })
  
  # perform function
  o <- try(do.call(main, pSorted))
  save(o, file = file.path(outputPath, paste0(filebase[1],iterId,".Rdata")))
  setwd(oldwd)
  
  return(invisible(TRUE))
}
