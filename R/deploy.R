#' Deploy the Shiny App
#' 
#' @description Deploy (copy) the shiny application to the specified directory
#'
#' @param directory [[character]] A character vector of one path to the new location.
#' @param app_name [[character]] A character vector defining the shiny application name in the new location.
#' @param demo [[logical]] A logical indicating if examples spreadsheets should be copied in the app.
#' @param overwrite [[logical]] A logical indicating if existing destination files should be overwritten.
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
deploy <- function(directory = "/srv/shiny-server", app_name = "insane", demo = FALSE, overwrite = FALSE) {
  dir.create(file.path(directory, app_name), showWarnings = FALSE, recursive = TRUE)
  
  out <- all(file.copy(
    from = list.files(system.file("app", package = "insane"), full.names = TRUE),
    to = file.path(directory, app_name),
    overwrite = overwrite, 
    recursive = TRUE
  ))
  
  if (!demo) {
    unlink(normalizePath(file.path(directory, app_name, "www", "xlsx")), recursive = TRUE)
  }
  dir.create(file.path(directory, app_name, "www", "xlsx"), showWarnings = FALSE, recursive = TRUE)
  Sys.chmod(normalizePath(file.path(directory, app_name, "www")), use_umask = FALSE)
  Sys.chmod(normalizePath(file.path(directory, app_name, "www", "xlsx")), use_umask = FALSE)
  
  out
}
