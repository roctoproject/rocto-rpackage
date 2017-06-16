# The functions for rocto

.onLoad <- function(libname, pkgname) {
  cat("\n                        __
       _________  _____/ /_____
      / ___/ __ \\/ ___/ __/ __ \\
     / /  / /_/ / /__/ /_/ /_/ /
    /_/   \\____/\\___/\\__/\\____/
    \n\n")

  cat(packageDescription("rocto", fields = "Title"))
}


# Create skeleton for a new rocto simulation
#' @export
job.skeleton <- function(name = "rocto-simulation",
                         path = ".", 
                         edit = TRUE,
                         changewd = FALSE) {
  
  safe.dir.create <- function(path) {
    if (!dir.exists(path) && !dir.create(path)) 
      stop(gettextf("cannot create directory '%s'", path), 
           domain = NA)
  }
  template_path <- file.path(find.package("rocto"), "template")
  files_to_copy <- list.files(template_path, full.names = TRUE)
  message("Creating directories ...", domain = NA)
  dir <- file.path(path, name)
  safe.dir.create(dir)
  b <- try(file.copy(files_to_copy, dir, recursive = TRUE))
  if (inherits(b, "try-error")) {
    stop(sprintf("Failed to create template in dir %s", dir))
  }
  if (edit) {
    file.edit(file.path(dir, "main.R"))
    file.edit(file.path(dir, "params.R"))
  }
  if (changewd) {
    setwd(dir)
  }
}


# Check whether directory is a valid job
.checkJob <- function(dir) {
  wrns <- msgs <- c()
  if (!dir.exists(dir)){
    wrns <- c(wrns, "Job directory does not exist")
  } else {
    lst <- list.files(dir)
    fileExp <- c("main.R", "params.R") 
    fileChk <- fileExp %in% lst
    if (!all(fileChk)){
      wrns <- c(wrns, sprintf("Missing file: ", fileExp[!fileChk]))
    } else {
      # create environment to evaluate the functions in main and params
      paramEnv <- new.env()
      mainEnv <- new.env()
      source(file.path(dir, "main.R"), mainEnv)
      source(file.path(dir, "params.R"), paramEnv)
      
      # check that all params are used in main
      parExp <- ls(paramEnv)
      parUse <- names(formals(mainEnv$main))
      parChk <- parExp %in% parUse
      if (!all(parChk)){
        wrns <- c(wrns, sprintf("Unused parameter in main: %s", parExp[!parChk]))
      }
      
      parChk <- parUse %in% parExp
      if (!all(parChk)){
        msgs <- c(msgs, sprintf("Parameter used in main but not iterated: %s", parExp[!parChk]))
      }
    }
    if (!dir.exists(file.path(dir, "data"))){
      wrns <- append(wrns, "Data directory does not exist")
    }
  }
  
  
  
  if (length(wrns) > 0) {
    # warn and return false
    for (w in wrns) {
      warning(w, call. = FALSE)
    }
    for (m in msgs) {
      message(m)
    }
    res <- FALSE
    attr(res, "warnings") <- wrns
  } else {
    # TODO ask user to continue
    for (m in msgs) {
      message(m)
    }
    res <- TRUE
  }
  
  return(invisible(res))
}
