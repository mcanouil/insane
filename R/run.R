#' Run the `"insane"` Shiny Application
#'
#' Runs a Shiny application. This function normally does not return; 
#' interrupt R to stop the application (usually by pressing Ctrl+C or Esc). 
#'
#' @inheritParams shiny::runApp
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   library(insane)
#'   # Must be run in an interactive R session!
#'   go_insane()
#' }
#'
go_insane <- function(
  port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"), 
  workerId = "",
  quiet = FALSE, 
  display.mode = c("auto", "normal", "showcase"),
  test.mode = getOption("shiny.testmode", FALSE)
) {
  if (!interactive()) {
    stop('[insane] Must be run in an interactive R session!')
  }
  shiny::runApp(
    appDir = system.file("app", package = "insane"), 
    port = port,
    launch.browser = launch.browser,
    host = host,
    workerId = workerId,
    quiet = quiet,
    display.mode = display.mode,
    test.mode = test.mode
  )
}
