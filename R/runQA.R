##' Start LCMS QA application
##' @param BPPARAM A [BiocParallel::BiocParallelParam] object
##' @return This function normally does not return; interrupt R to stop the
##'   application (usually by pressing Ctrl + C or ESC)
##' @examples
##'
##' \donttest{runQA()}
##'
##' @export
runQA <- function() {
  ## if (!("xcms" %in% (.packages()))) {
  ##   suppressPackageStartupMessages(require(xcms))
  ## }
  ## Enable js
  useShinyjs() ## need to initiate
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
