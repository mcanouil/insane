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
#' @importFrom broom tidy
#' @importFrom dplyr `%>%` tibble mutate filter group_by ungroup do full_join n select transmute arrange summarise distinct bind_rows matches num_range across
#' @importFrom DT dataTableOutput renderDataTable
#' @import ggplot2
#' @import ggpubr
#' @importFrom patchwork plot_annotation wrap_plots
#' @importFrom glue glue glue_collapse
#' @importFrom purrr set_names pmap map map2 map2_dbl map2_chr map2_df
#' @importFrom readxl read_xlsx
#' @importFrom shiny tags NS downloadButton callModule downloadHandler navbarPage tabPanel column tagList selectInput radioButtons helpText sliderInput fluidRow numericInput downloadLink downloadLink fileInput uiOutput sidebarLayout sidebarPanel mainPanel actionButton icon actionLink observeEvent showModal modalDialog observe req hideTab showTab reactive renderUI showNotification removeNotification shinyApp runApp
#' @importFrom stats quantile IQR lm na.omit var update.formula
#' @importFrom tidyr unnest pivot_wider pivot_longer drop_na replace_na nest
#' @importFrom utils getFromNamespace combn
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
  demo = FALSE,
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

  app_name <- paste0("app_", floor(as.numeric(Sys.time())))
  on.exit(unlink(file.path(tempdir(), app_name), recursive = TRUE))
  deploy(directory = tempdir(), app_name = app_name, demo = demo)

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
}
