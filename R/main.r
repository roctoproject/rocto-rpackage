# The functions for rocto

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("                        __
       _________  _____/ /_____
      / ___/ __ \\/ ___/ __/ __ \\
     / /  / /_/ / /__/ /_/ /_/ /
    /_/   \\____/\\___/\\__/\\____/\n")
  packageStartupMessage(paste0("    ", utils::packageDescription("rocto", fields = "Title")),"\n")
}


#' Initialise a new rocto job
#' 
#' Creates a skeleton for a new \code{rocto} job in the correct format. Also allows you
#' to automatically set the working directory and open the param and main files.
#' 
#' @param name <character> Name to be given to the new job directory
#' @param path <character> Where to create the directory (default to current working directory)
#' 
#' @return Invisible boolean TRUE
#' 
#' @seealso \code{\link{packJob}}
#'  
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
  edit <- utils::menu(c("Yes", "No"), title="Open main and param files?")
  if (edit == 1) {
    file.edit(file.path(dir, "main.R"))
    file.edit(file.path(dir, "params.R"))
  }
  if (changewd == 1) {
    setwd(dir)
  }
  return(invisible(TRUE))
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
#' @seealso \code{\link{newJob}}, \code{\link{resultsToList}}
#'  
#' @export
packJob <- function(path = ".", verbose = FALSE) {
  initwd <- getwd()
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
#' @seealso \code{\link{newJob}}, \code{\link{packJob}}
#'  
#' @export
resultsToList <- function(roctoResults) {
  if (!dir.exists(roctoResults)){
    stop("Results directory not found")
  }
  return(lapply(list.files(roctoResults, full.names = TRUE), 
                function(f) {load(f); return(o)}))
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
      mainSourced <- try(source("main.R", mainEnv), silent = TRUE)
      paramsSourced <- try(source("params.R", paramEnv), silent = TRUE)
      
      if (inherits(mainSourced, "try-error")) {
        # remove call
        w <- trimws(sub("[^:]*: ", "", mainSourced[1], perl = TRUE)) 
        wrns <- c(wrns, w)
        if (inherits(paramsSourced, "try-error")) {
          # remove call
          w <- trimws(sub("[^:]*: ", "", paramsSourced[1], perl = TRUE)) 
          wrns <- c(wrns, w)
        }
      }
      
      if (!inherits(mainSourced, "try-error") && 
          !inherits(paramsSourced, "try-error")) {
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
  colnames(grid) <- names(gridList)
  save(grid, file = "grid.Rdata")
  
  # create meta information
  meta <- list(
    "nParams" = ncol(grid), 
    "params" = colnames(grid),
    "testParams" = gridEnv[["testParams"]],
    "nIter" = nrow(grid), 
    "dataSize" = file.size("data"), 
    "RInfo" = as.list(unlist(version)),
    "RPackages" = list(
      
    ))
  
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
  filename <- paste0(basename(fulldir), ".rocto")
  if (file.exists(filename)) {
    unlink(filename)
  }
  zip::zip(filename, basename(fulldir), recurse = TRUE)
  file.copy(from = filename, to = dirname(fulldir))
  open <- utils::menu(c("Yes", "No"), title="Open containing folder?")
  if (open == 1) {
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


.findUsedPackages <- function(file, namesOnly = FALSE) {
  # Determine packages used
  if (!class(text)=="character")
    stop("Input a string")
  
  text <- paste(readLines(file, warn = FALSE),collapse="\n")

  # Init
  
  # Check if this file sources other files
  regex <- "(?<=source\\([\\\"\\']).*(?=[\\\"\\']\\))"
  matches <- gregexpr(regex, text, perl = TRUE)[[1]]
  lengths <- attr(matches, "match.length")
  
  # If it does, recursively get the names of packages from those files
  sourcedPackages <- list()
  if (any(matches>=0)){
    for (m in seq_along(matches)){
      sourceFile <- substr(text,matches[m],matches[m]+lengths[m]-1)
      sourcedPackages[[m]] <- .findUsedPackages(sourceFile, namesOnly = TRUE)
    }
  }
    
  # Find packages used in this file
  regex <- "(?<=library\\().*(?=\\))|(?<=require\\().*(?=\\))|(?<=[ \\t\\n\\(\\{\\|\\&\\)\\}\\\"\\'])[A-Za-z0-9\\.]*(?=::)"
  matches <- gregexpr(regex, text, perl = TRUE)[[1]]
  lengths <- attr(matches, "match.length")
  
  # If there are any, get their names
  usedPackages <- NULL
  if (any(matches>=0)){
    for (m in seq_along(matches)){
      usedPackages[m] <- substr(text,matches[m],matches[m]+lengths[m]-1)
    }
  }
  
  usedPackages <- trimws(c(usedPackages, 
                           unlist(sourcedPackages, use.names = FALSE)))
  
  if (namesOnly) return(usedPackages)
  
  if (length(usedPackages)>0) {
    
    # Get the version number of each package and return output
    uniquePackages <- unique(usedPackages)
    pkgElement <- list("name"=NULL, "version"=NULL)
    out <- rep(list(pkgElement), length(uniquePackages))
    
    for (p in seq_along(uniquePackages)){
      pkg <- uniquePackages[p]
      ver <- as.character(utils::packageVersion(pkg))
      out[[p]][["name"]] <- pkg
      out[[p]][["version"]] <- ver
    }
    return(out)
    
  } else {
    
    # no packages used, return null
    return(NULL)
    
  }
}
