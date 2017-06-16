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
  wrns <- c()
  if (!dir.exists(dir)){
    wrns <- append(wrns, "Job directory does not exist")
  } else {
    lst <- list.files(dir)
    fileExp <- c("main.R", "params.R") 
    fileChk <- fileexp %in% lst
    if (!all(fileChk)){
      wrns <- append(wrns, sprintf("Missing file: ", fileExp[fileChk]))
    } else {
      # create environment to evaluate the functions in main and params
      paramEnv <- new.env()
      mainEnv <- new.env()
      source(file.path(dir, "main.R"), mainEnv)
      source(file.path(dir, "params.R"), paramEnv)
      
      # check that all params are used in main
      parExp <- list(paramEnv)
      parChk <- parExp %in% names(formals(mainEnv$main))
      if (!all(parChk)){
        wrns <- append(wrns, sprintf("Unused param: ", parExp[parChk]))
      }
    }
    # if (!dir.exists(file.path(dir, "data"))){
    #   wrns <- append(wrns, "Data directory does not exist")
    # }
  }
  
  
  
  if (length(wrns) > 0) {
    # warn and return false
    for (w in wrns) {
      warning(w, call. = FALSE)
    }
    res <- FALSE
    attr(res, "warnings") <- wrns
  } else {
    res <- TRUE
  }
  
  return(res)
}
