# evaluate a function using a temporary working directory and optionally a 
# temporary environment
.withDir <- function(tdir, expr, envir = NULL) {
  oldWd <- getwd()
  on.exit(setwd(oldWd))
  setwd(normalizePath(tdir))
  evalq(expr, envir)
}

# Open a folder
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