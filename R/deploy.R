#' Deploy the Shiny App
#' 
#' @description Deploy (copy) the shiny application to the specified directory
#'
#' @param directory [[character]] A character vector of one path to the new location.
#' @param app_name [[character]] A character vector defining the shiny application name in the new location.
#' @param with_examples [[logical]] A boolean indicating if examples should be copied in the App.
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
deploy <- function(directory = "/srv/shiny-server", app_name = "insane", with_examples = FALSE) {
  dir.create(file.path(directory, app_name), showWarnings = FALSE, recursive = TRUE)
  out <- all(file.copy(
    from = list.files(system.file("app", package = "insane"), full.names = TRUE),
    to = file.path(directory, app_name),
    overwrite = TRUE, recursive = TRUE
  ))
  
  if (with_examples) {
    dir.create(file.path(directory, app_name, "www", "xlsx"), showWarnings = FALSE)
    all(file.copy(
      from = list.files(system.file("extdata", package = "insane"), full.names = TRUE, recursive = TRUE),
      to = file.path(directory, app_name, "www", "xlsx"),
      overwrite = TRUE, recursive = TRUE
    ))
  }
  
  out
}
