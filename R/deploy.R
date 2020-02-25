#' Deploy the Shiny App
#' 
#' @description Deploy (copy) the shiny application to the specified directory
#'
#' @param directory [[character]] A character vector of one path to the new location.
#' @param app_name [[character]] A character vector defining the shiny application name in the new location.
#'
#' @return [[logical]] A logical indicating whether the deployment is successfull (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#'
#' deploy(directory = ".")
#'
#' if (interactive()) {
#'   shiny::runApp("insane")
#' }
#'
deploy <- function(directory = "/srv/shiny-server", app_name = "insane") {
  dir.create(file.path(directory, app_name), showWarnings = FALSE, recursive = TRUE)
  all(file.copy(
    from = list.files(system.file("app", package = "insane"), full.names = TRUE),
    to = file.path(directory, app_name),
    overwrite = TRUE, recursive = TRUE
  ))
}
