##' Start LC/MS QA application
##'
##' Runs a Shiny application to check the quality of LC/MS experiment. The
##' system's default web browser will be launched automatically after the app is
##' started.
##'
##' @return This function normally does not return; interrupt R to stop the
##'   application (usually by pressing Ctrl + C or ESC)
##' @examples
##'
##' if (interactive() {
##'   runQA()
##' })
##'
##' @export
runQA <- function() {
  ## Enable js
  useShinyjs() ## need to initiate
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
