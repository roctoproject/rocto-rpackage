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

  fileBase <- basename(roctoJob)
  fullPath <- normalizePath(roctoJob)
  tdir <- tempdir()

  if (dir.exists(fullPath)) {
    # roctoJob is a directory, no unzip needed
    # check if grid.json, meta.json, and main.R exist
    if (!all(file.exists(file.path(fullPath, "grid.json")),
             file.exists(file.path(fullPath, "meta.json")),
             file.exists(file.path(fullPath, "main.R")))) {
      stop(sprintf("%s is not an extracted rocto folder. Run roctoPack().",
                   roctoJob))
    }
    copyPath <- file.path(tdir, fileBase)
    if (file.exists(copyPath)) {
      unlink(copyPath, recursive = TRUE)
    }
    dir.create(copyPath)
    if (!all(file.copy(list.files(fullPath, include.dirs = TRUE,
                                  full.names = TRUE),
                       copyPath, recursive = TRUE))) {
      stop("Folder could not be copied to temporary directory.")
    }
  } else if (file.exists(fullPath)) {
    # roctoJob is a file, unzip needed
    ext <- substr(fileBase,
                  gregexpr("\\.(?!.*\\.)", fileBase, perl = TRUE)[[1]][1],
                  nchar(fileBase))
    if (tolower(ext) != ".rocto") {
      stop(sprintf("%s is not a .rocto file.", roctoJob))
    }
    fileBase <- substr(fileBase, 1, gregexpr("\\.(?!.*\\.)",
                                             fileBase,
                                             perl = TRUE)[[1]][1] - 1)
    copyPath <- file.path(tdir, fileBase)
    if (file.exists(copyPath)) {
      unlink(copyPath, recursive = TRUE)
    }
    uz <- try(utils::unzip(fullPath, exdir = tdir))
    if (length(uz) == 0) {
      stop("Unzip failed")
    }
  } else {
    stop("Rocto job folder or file does not exist.")
  }

  outputPath <- suppressWarnings(normalizePath(outputDir)) # TODO gracefully catch "dir does not exist"
  if (!dir.exists(outputPath)) {
    dir.create(outputPath, recursive = TRUE)
  }

  o <- .runJob(file.path(copyPath), iterId)
  save(o, file = file.path(outputPath, paste0(fileBase,"-",iterId,".Rdata")))

  return(invisible(TRUE))
}
