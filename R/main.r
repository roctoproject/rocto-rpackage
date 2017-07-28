# User-facing functions for rocto

#' Initialise a new rocto job
#' 
#' Creates a skeleton for a new \code{rocto} job in the correct format. Also allows you
#' to automatically set the working directory and open the param and main files.
#' 
#' @param name <character> Name to be given to the new job directory
#' @param path <character> Where to create the directory (default to current working directory)
#' @param interactive <boolean> Whether to ask for feedback from user.
#' 
#' @return Invisible boolean TRUE
#' 
#' @seealso \code{\link{roctoCheck}} \code{\link{roctoPack}}
#'  
#' @export
roctoNew <- function(name = "roctoJob",
                     path = ".",
                     interactive = TRUE) {
    if (dir.exists(file.path(path,name))) {
      if (interactive) {
        over <-  utils::menu(c("Yes", "No"), 
                             title = "Directory already exists. Overwrite?")
      } else {
        over <- 1
      }
      if (over == 1) {
        unlink(file.path(path, name), recursive = TRUE)
      } else {
        cat("Cancelled")
        return(invisible(FALSE))
      }
    }
    safe.dir.create <- function(path) {
      if (!dir.exists(path) && !dir.create(path)) 
        stop(gettextf("cannot create directory '%s'", path), 
             domain = NA)
    }
    template_path <- system.file("template", package = "rocto")
    files_to_copy <- list.files(template_path, full.names = TRUE)
    dir <- file.path(path, name)
    safe.dir.create(dir)
    b <- try(file.copy(files_to_copy, dir, recursive = TRUE))
    if (inherits(b, "try-error")) {
      stop(sprintf("Failed to create template in dir %s", dir))
    }
    
    if (interactive) {
      changewd <- utils::menu(c("Yes", "No"), 
                              title = "Set working directory to created folder?")
      edit <- utils::menu(c("Yes", "No"), 
                          title = "Open main and param files?")
    } else {
      changewd <- edit <- 0
    }
    if (edit == 1) {
      get("file.edit")(file.path(dir, "main.R"))
      get("file.edit")(file.path(dir, "params.R"))
    }
    if (changewd == 1) {
      setwd(dir)
    }
    return(invisible(TRUE))
  }



#' Check a rocto job
#' 
#' Checks whether an existing \code{rocto} job contains any errors.
#' It also specifies where problems may be and warns about unexpected deviations
#' from the default \code{rocto} folder format.
#' 
#' @param path <character> Path to existing \code{rocto} directory (default to current working directory)
#' 
#' @return Invisible boolean valid job or not.
#' 
#' @seealso \code{\link{roctoNew}} \code{\link{roctoPack}}
#'  
#' @export
roctoCheck <- function(path=".") {
  tdir <- tempdir()
  valid <- .checkJob(path, tdir, interactive = FALSE)
  if (valid) {
    cat("\nYour rocto job is valid.")
  }
  return(invisible(valid == TRUE))
}


#' Pack a rocto directory for distribution
#' 
#' This function performs checks and then packs a raw \code{rocto} job directory 
#' into a single file for uploading to the \code{rocto} volunteer cluster.
#' 
#' @param path <character> The root of the raw rocto job directory.
#' @param verbose <boolean> Print debug information for inspecting parameter grid and meta info
#' 
#' @return Invisible boolean TRUE
#' 
#' @seealso \code{\link{roctoCheck}}, \code{\link{roctoResults}}
#'  
#' @export
roctoPack <- function(path = ".", verbose = TRUE) {
  initwd <- getwd()
  validJob <- jobPrepped <- jobPacked <- FALSE
  tdir <- tempdir()
  # first, check whether directory is a valid job
  validJob <- .checkJob(path, tdir)
  if (validJob) {
    cat("\nYour rocto job is valid.")
    # prepare job for packing and gather information
    jobPrepped <- .prepJob(path, tdir)
    if (jobPrepped) {
      cat("\nJob information saved.\n")
      # package the job, copy it next to the original and ask to open folder
      jobPacked <- .zipJob(path, tdir, verbose)
    }
  }
  
  if (validJob && jobPrepped && jobPacked) {
    cat("Job successfully packed.")
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}

#' Load results from your rocto job into R
#' 
#' This function loads all the job results from a results folder into R at once.
#' 
#' @param roctoResults <character> Path to a rocto results folder.
#' 
#' @return List with \code{nIter} elements, each containing the results object of one iteration.
#' 
#' @seealso \code{\link{roctoNew}}, \code{\link{roctoPack}}
#'  
#' @export
roctoResults <- function(roctoResults) {
  if (!dir.exists(roctoResults)) {
    stop("Results directory not found")
  }
  return(lapply(list.files(roctoResults, full.names = TRUE), 
                function(f) {load(f); return(get("o"))}))
}
