#' Run the Shiny Application
#'
#' @description 
#' Runs a Shiny application. This function normally does not return; 
#' interrupt R to stop the application (usually by pressing Ctrl+C or Esc). 
#'
#' @inheritParams shiny::runApp
#' @inheritParams deploy
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
  with_examples = FALSE,
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

  # nocov start
  app_name <- paste0("app_", floor(as.numeric(Sys.time())))
  on.exit(unlink(file.path(tempdir(), app_name), recursive = TRUE))
  deploy(directory = tempdir(), app_name = app_name, with_examples = with_examples)

  shiny::runApp(
    appDir = file.path(tempdir(), app_name), 
    port = port,
    launch.browser = launch.browser,
    host = host,
    workerId = workerId,
    quiet = quiet,
    display.mode = display.mode,
    test.mode = test.mode
  )
  # nocov end
}
