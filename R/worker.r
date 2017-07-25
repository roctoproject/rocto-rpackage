# Worker-related functions for rocto

#' Evaluate one iteration from a rocto job.
#' 
#' This function runs one rocto job iteration - a "task". This function is
#' usually not called by the user - it is used by the desktop app to run 
#' rocto tasks. Nevertheless, a user can call this function to try out their 
#' rocto job.
#' 
#' @param roctoJob <character> Path to a \code{.rocto} file containing the job.
#' @param outputDir <character> Full directory where the output file should be saved.
#' @param iterId <integer> or <character: "test"> Indicates which task \code{[1:nIter]}
#' should be run, or whether the test parameters should be used.
#' 
#' @return Invisible boolean TRUE.
#' 
#' @seealso \code{\link{roctoNew}}, \code{\link{roctoPack}}, \code{\link{roctoResults}}
#'  
#' @export
roctoRun <- function(roctoJob, outputDir, iterId = "test"){
  filebase <- strsplit(basename(roctoJob), "\\.")[[1]]
  if (tolower(filebase[2]) != "rocto") {
    stop(sprintf("%s is not a .rocto file.", roctoJob))
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
  
  tempwd <- file.path(tdir, filebase[1])
  o <- .runJob(tempwd, iterId)
  save(o, file = file.path(outputPath, paste0(filebase[1],"-",iterId,".Rdata")))
  
  return(invisible(TRUE))
}
