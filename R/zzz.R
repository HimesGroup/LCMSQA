.onLoad <- function(...){
  suppressPackageStartupMessages(require("MSnbase", quietly = TRUE))
  suppressPackageStartupMessages(require("BiocParallel", quietly = TRUE))
  packageStartupMessage("Welcome to the LCMSQA package!")
}
