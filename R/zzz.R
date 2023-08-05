.onAttach <- function(...) {
  ## BioC dependency check
  bioc_dep <- c("MSnbase", "xcms")
  dep_installed <- sapply(bioc_dep, function(x) requireNamespace(x, quietly = TRUE))
  if (any(!dep_installed)) {
    packageStartupMessage(
      "LCMSQA needs the following BioC package(s): ",
      toString(bioc_dep[!dep_installed]), "\n",
      "Try first:\n" ,
      "BiocManager::install(c(",
      paste(gsub("(\\w+)", '"\\1"', bioc_dep[!dep_installed]), collapse = ", "),
      "))"
    )
  } else{
    ## Package startup message
    packageStartupMessage("Welcome to the LCMSQA package!")
  }
}
