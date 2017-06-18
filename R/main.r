# The functions for rocto

.onLoad <- function(libname, pkgname) {
  cat("\n                        __
       _________  _____/ /_____
      / ___/ __ \\/ ___/ __/ __ \\
     / /  / /_/ / /__/ /_/ /_/ /
    /_/   \\____/\\___/\\__/\\____/
    \n\n")
  
  cat("    ")
  cat(packageDescription("rocto", fields = "Title"))
  cat("\n ")
}


# Create skeleton for a new rocto job
#' @export
newJob <- function(name = "roctoJob",
                   path = ".") {
  if (dir.exists(file.path(path,name))){
    over <-  utils::menu(c("Yes", "No"), title="Directory already exists. Overwrite?")
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
  changewd <- utils::menu(c("Yes", "No"), title="Set working directory to created folder?")
  if (changewd == 1) {
    setwd(dir)
  }
  edit <- utils::menu(c("Yes", "No"), title="Open main and param files?")
  if (edit == 1) {
    file.edit(file.path(dir, "main.R"))
    file.edit(file.path(dir, "params.R"))
  }
  
}

# Pack the job for uploading to the rocto server
#' @export
packJob <- function(path = ".", verbose = FALSE) {
  initwd <- getwd()
  on.exit(function(){ setwd(initwd) })
  validJob <- jobPrepped <- jobPacked <- FALSE
  tdir <- tempdir()
  # first, check whether directory is a valid job
  validJob <- .checkJob(path, tdir)
  if (validJob) {
    # prepare job for packing and gather information
    jobPrepped <- .prepJob(path, tdir, verbose)
    if (jobPrepped) {
      # package the job, copy it next to the original and ask to open folder
      jobPacked <- .zipJob(path, tdir)
    }
  }
  
  if (validJob && jobPrepped && jobPacked) {
    cat("Job successfully packed.")
    return(invisible(TRUE))
  } else {
    stop("Something went wrong during packaging. Inspect the warnings for more info.")
  }
  
}


# Check whether directory is a valid job
.checkJob <- function(dir, tdir) {
  oldwd <- getwd()
  fulldir <- normalizePath(dir)
  wrns <- msgs <- c()
  if (!dir.exists(dir)){
    wrns <- c(wrns, "Job directory does not exist")
  } else {
    # first copy to tempdir and switch to it.
    copySuccess <- file.copy(fulldir, tdir, recursive = TRUE)
    if (copySuccess) {
      setwd(file.path(tdir, basename(fulldir)))
    } else {
      stop("Temporary directory not available; could not check your package. Perhaps you don't have the correct permissions.")
    }
    
    lst <- list.files()
    fileExp <- c("main.R", "params.R") 
    fileChk <- fileExp %in% lst
    if (!all(fileChk)){
      wrns <- c(wrns, sprintf("Missing file: ", fileExp[!fileChk]))
    } else {
      # create environment to evaluate the functions in main and params
      paramEnv <- new.env()
      mainEnv <- new.env()
      source("main.R", mainEnv)
      source("params.R", paramEnv)
      
      # check that testParams exist in the params file and that they contain all
      # iterated parameters
      parItr <- ls(paramEnv)
      if (!"testParams" %in% parItr) {
        wrns <- c(wrns, "testParams not found!")
      } else {
        parItr <- parItr[parItr!="testParams"]
        parTst <- names(paramEnv$testParams)
        if (!all(sort(parItr) == sort(parTst))) {
          wrns <- c(wrns, "Elements of testParams are not the same as iterated params!")
        } else {
          for (p in parTst) {
            if(class(paramEnv$testParams[[p]]) != class(paramEnv[[p]])) {
              wrns <- c(wrns, sprintf("testParam '%s' does not have the same class as its iterated counterpart!", p))
            }
          }
        }
      }
      
      # check that all params are used in main and all main params are iterated
      parUse <- names(formals(mainEnv$main))
      parChk <- parItr %in% parUse
      if (!all(parChk)){
        wrns <- c(wrns, sprintf("Unused parameter in main: %s", parItr[!parChk]))
      }
      
      parChk <- parUse %in% parItr
      if (!all(parChk)){
        wrns <- c(wrns, sprintf("Parameter used in main but not iterated: %s", 
                                parUse[!parChk]))
      }
    }
    
    if (!dir.exists("data")){
      msgs <- c(msgs, "Data directory does not exist")
    }
  }
  
  
  # Check for warnings and messages and return result
  if (length(wrns) > 0) {
    cat("\nJob package check failed! Inspect the warning messages and adjust your code accordingly.")
    for (w in wrns) {
      warning(w, call. = FALSE)
    }
    for (m in msgs) {
      message(m)
    }
    message("")
    res <- FALSE
    attr(res, "warnings") <- wrns
    attr(res, "messages") <- msgs
  } else {
    if (length(msgs) > 0) {
      for (m in msgs) {
        message(m)
      }
      message("")
      cont <- utils::menu(c("Yes", "No"), title="Proceed anyway?")
      if (cont == 1) {
        res <- TRUE
        attr(res, "messages") <- msgs
      } else {
        res <- FALSE
        attr(res, "messages") <- msgs
      }
    }
    res <- TRUE
  }
  # Remove tempdir, switch back to original working directory and return output
  setwd(oldwd)
  unlink(file.path(tdir, basename(fulldir)), recursive = TRUE)
  return(invisible(res))
}


# Prepare job for packing and gather information
.prepJob <- function(dir, tdir, verbose = FALSE) {
  fulldir <- normalizePath(dir)
  oldwd <- getwd()
  # first copy to tempdir and switch to it.
  copySuccess <- file.copy(fulldir, tdir, recursive = TRUE)
  if (copySuccess) {
    setwd(file.path(tdir, basename(fulldir)))
  } else {
    stop("Temporary directory not available; could not prepare your package. Perhaps you don't have the correct permissions.")
  }
  
  # create the parameter grid
  gridEnv <- new.env()
  source("params.R", gridEnv)
  gridList <- list()
  for (p in ls(gridEnv)[ls(gridEnv)!="testParams"]){
    gridList[[p]] <- gridEnv[[p]]
  }
  grid <- expand.grid(gridList, stringsAsFactors = FALSE)
  save(grid, file = "grid.Rdata")
  
  # create meta information
  meta <- list(
    "nParams" = ncol(grid), 
    "params" = colnames(grid),
    "testParams" = gridEnv[["testParams"]],
    "nIter" = nrow(grid), 
    "dataSize" = file.size("data"), 
    "RInfo" = as.list(unlist(version)))
  
  jsonMeta <- jsonlite::toJSON(meta, pretty = TRUE)
  write(jsonMeta, file = "meta.json")
  
  if (verbose) {
    print(grid)
    print(jsonMeta)
  }
  
  setwd(oldwd)
  return(invisible(TRUE))
}

# Package the job, copy it next to the original folder and ask to open folder
.zipJob <- function(dir, tdir) {
  fulldir <- normalizePath(dir)
  oldwd <- getwd()
  setwd(tdir)
  zip::zip(paste0(basename(fulldir), ".rocto"), 
           basename(fulldir), recurse = TRUE)
  file.copy(from = paste0(basename(fulldir), ".rocto"), to = dirname(fulldir))
  open <- utils::menu(c("Yes", "No"), title="Open containing folder?")
  if (open) {
    .openFolder(dirname(fulldir))
  }
  setwd(oldwd)
  return(invisible(TRUE))
}

.openFolder <- function(pathname = ".") {
  os <- Sys.info()['sysname']
  if (os == "Windows") {
    shell.exec(normalizePath(pathname))
  } else if (os == "Darwin") {
    system2("open", normalizePath(pathname))
  } else if (os == "Linux") {
    system(paste0("xdg-open ", normalizePath(pathname))) 
  }
}
