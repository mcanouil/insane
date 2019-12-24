invisible(suppressPackageStartupMessages({
  sapply(
    c("shiny", "DT", "rlang", "ggplot2", "scales", "purrr", "dplyr", "tidyr", "glue", "readxl", "stats"),
    library, character.only = TRUE
  )
}))

source("utils.R")

# UI-side ==========================================================================================
ui <- shiny::navbarPage(
  theme = "yeti-bootstrap.min.css",
  title = "EndoC-β",
  windowTitle = "EndoC-β",
  collapsible = TRUE,
  id = "main-menu",
  selected = "blank-tab",
  ## Upload tab ------------------------------------------------------------------------------------
  shiny::tabPanel("Upload Experiments & Plot Settings", icon = shiny::icon("file-upload"), value = "upload-tab",
    shiny::fluidRow(
      shiny::column(width = 6,
        card(title = "Plot Settings", body = {
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(6, align = "center",
                shiny::numericInput("font_size", shiny::tags$span("Font Size", shiny::helpText("(pt)")),
                  value = 16
                )
              ),
              shiny::column(6, align = "center",
                shiny::numericInput("point_size", shiny::tags$span("Point Size", shiny::helpText("(mm)")),
                  value = 2,
                  min = 0, max = 4, step = 0.5
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(4, align = "center",
                shiny::numericInput("plot_width", shiny::tags$span("Width", shiny::helpText("(cm)")),
                  value = 16
                )
              ),
              shiny::column(4, align = "center",
                shiny::numericInput("plot_height", shiny::tags$span("Height", shiny::helpText("(cm)")),
                  value = 12
                )
              ),
              shiny::column(4, align = "center",
                shiny::numericInput("plot_dpi", shiny::tags$span("DPI", shiny::helpText("(Default: 120)")),
                  value = 120
                )
              )
            )
          )
        })
      ),
      shiny::column(width = 6,
        card(title = "Upload Experiments in Excel Files", body = {
          shiny::fileInput("xlsx_files", "Choose One or Several Excel Files", 
            multiple = TRUE, width = "34%",
            accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
          )
        })
      )
    ),
    shiny::uiOutput("upload_ui")
  ),
  ## Blank tab -------------------------------------------------------------------------------------
  shiny::tabPanel("Blank Quality-Control", icon = shiny::icon("chart-line"), value = "blank-tab", 
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3, 
        shiny::tags$div(align = "center", 
          sliderInput("od_outlier", 
            shiny::tags$span("Optical Density (OD) Outliers Threshold", 
              shiny::helpText(
                "(Higher than", shiny::tags$strong("X"), 
                "times the interquartile range above the 75", shiny::tags$sup("th", .noWS = "before"), "percentile",
                "or",
                "lower than", shiny::tags$strong("X"), 
                "times the interquartile range below the 25", shiny::tags$sup("th", .noWS = "before"), "percentile)"
              )
            ),
            min = 1, max = 3, value = 1.5, step = 0.25
          ), 
          sliderInput("lm_outlier", 
            shiny::tags$span("Blank Estimates Outliers Threshold", 
              shiny::helpText(
                "(Higher than", shiny::tags$strong("X"), 
                "times the interquartile range above the 75", shiny::tags$sup("th", .noWS = "before"), "percentile",
                "or",
                "lower than", shiny::tags$strong("X"), 
                "times the interquartile range below the 25", shiny::tags$sup("th", .noWS = "before"), "percentile)"
              )
            ),
            min = 1, max = 3, value = 1.5, step = 0.25
          )
        )
      ), 
      shiny::mainPanel(width = 9,
        shiny::fluidRow(
          shiny::column(width = 6, align = "center", 
            plotDownloadInputUI("od_box_plot", "Optical Density (OD) Relative Error Outliers")
          ),
          shiny::column(width = 6, align = "center", 
            plotDownloadInputUI("od_density_plot", "Optical Density (OD) Relative Error Distribution")
          )
        ), 
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 6, align = "center",
            plotDownloadInputUI("od_lm_box_plot", "Blank Linear Regression Outliers")
          ),
          shiny::column(width = 6, align = "center",
            plotDownloadInputUI("od_lm_line_plot", "Blank Linear Regression")
          )
        )
      )
    )
  ),
  ## Fold-Change tab -------------------------------------------------------------------------------
  shiny::tabPanel("Fold-Change Quality-Control", icon = shiny::icon("chart-area"), value = "foldchange-tab",
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::tags$div(align = "center",
          sliderInput("fold_change",
            shiny::tags$span('Fold Change Threshold to Define "Secretion"',
              shiny::helpText(
                "(Threshold above which cells are allegedly secreting insulin)"
              )
            ),
            min = 0.5, max = 2, value = 1.1, step = 0.05
          )
        )
      ),
      shiny::mainPanel(width = 9,
        shiny::fluidRow(
          shiny::column(width = 12, align = "center",
            plotDownloadInputUI("fold_change_plot", "Fold-Change of Insulin Secretion Distribution", height = "780px")
          )
        )
      )
    )
  ),
  ## Analysis tab ----------------------------------------------------------------------------------
  shiny::tabPanel("Insulin Secretion Analysis", icon = shiny::icon("chart-bar"), value = "is_analysis-tab", 
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3, 
        shiny::tags$div(align = "center", 
          shiny::radioButtons("search_by", "Search by", 
            choices = c("Target", "File"), selected = "Target", 
            inline = TRUE
          ), 
          shiny::uiOutput("sheet_ui"),
          shiny::uiOutput("experiment_ui")
        )
      ), 
      shiny::mainPanel(width = 9,
        shiny::fluidRow(
          shiny::column(width = 6, align = "center",
            plotDownloadInputUI("blank_plot", "Blank")
          ),
          shiny::column(width = 6, align = "center",
            plotDownloadInputUI("is_foldchange_plot", "Fold-Change (Target(s)/Control)")
          )
        ),
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 12, align = "center",
            plotDownloadInputUI("is_plot", "Insulin Secretion")
          )
          # shiny::column(width = 4, align = "center", 
          #   plotDownloadInputUI("is_od_plot", "is_od_plot")
          # ),
          # shiny::column(width = 4, align = "center", 
          #   plotDownloadInputUI("is_percent_plot", "is_percent_plot")
          # ),
          # shiny::column(width = 4, align = "center", 
          #   plotDownloadInputUI("is_logratio_plot", "is_logratio_plot")
          # )
        )
      )
    )
  )
)

# Server-side ======================================================================================
server <- function(input, output, session) {
  shiny::observe({
    shiny::req(input[["plot_dpi"]], input[["plot_height"]], input[["plot_dpi"]])
    purrr::map2(
      .x = list(
        "od_box_plot", "od_density_plot", "od_lm_box_plot", "od_lm_line_plot", 
        "fold_change_plot",
        "is_od_plot", "is_percent_plot", "is_foldchange_plot",
        "blank_plot", "is_plot", "is_logratio_plot"
      ),
      .y = list(
        od_box_plot(), od_density_plot(), od_lm_box_plot(), od_lm_line_plot(), 
        fold_change_plot(),
        is_od_plot(), is_percent_plot(), is_foldchange_plot(),
        blank_plot(), is_plot(), is_logratio_plot()
      ),
      .f = function(.x, .y) {
        plotDownloadInput(
          id = .x, plot_object = .y, 
          width = input[["plot_width"]] %||% 16, 
          height = input[["plot_height"]] %||% 12, 
          dpi = input[["plot_dpi"]] %||% 120
        )
      }
    )
  })

  ## Upload tab ------------------------------------------------------------------------------------
  
  output$xlsx_contents_uploaded <- shiny::renderTable({ shiny::req(input$xlsx_files) })
  
  output$xlsx_contents_summary <- shiny::renderUI({
    xlsx_size <- sum(input$xlsx_files[, "size"])
    class(xlsx_size) <- "object_size"
    shiny::tags$p(
      "A total of", shiny::tags$strong(length(input$xlsx_files[, "name"])), "Excel files were succesfully uploaded,",
      "for a total amount of", shiny::tags$strong(format(xlsx_size, units = "Kb")), "."
    )
  })
  
  output$xlsx_available <- DT::renderDataTable({
    xlsx_contents() %>% 
      dplyr::group_by(file = basename(.data[["file"]])) %>% 
      dplyr::summarise_at(
        .vars = dplyr::vars(.data[["Date"]], .data[["Operator"]], .data[["sheet_name"]], .data[["Condition"]]), 
        .funs = ~ glue::glue_collapse(unique(stats::na.omit(.x)), sep = ", ", last = " and ")
      ) %>% 
      dplyr::ungroup()
  })
  
  output$upload_ui <- shiny::renderUI({
    list(
      if (is.null(input$xlsx_files)) {
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 12,
            card(title = "Summary", body = shiny::tags$p("No Excel files uploaded."))
          )
        )
      } else {
        list(
          shiny::fluidRow(style = "padding-top: 1em;",
            shiny::column(width = 12,
              card(title = "Summary", body = shiny::uiOutput("xlsx_contents_summary"))
            )
          ),
          shiny::fluidRow(style = "padding-top: 1em;",
            shiny::column(width = 12,
              card(title = "Excel Files Uploaded", body = shiny::tableOutput("xlsx_contents_uploaded"))
            )
          )
        )
      },
      shiny::fluidRow(style = "padding-top: 1em;",
        shiny::column(width = 12,
          card(title = "Experiments Available", body = DT::dataTableOutput("xlsx_available"))
        )
      )
    )
  })
  
  observeEvent(input$xlsx_files, {
    purrr::pmap(input$xlsx_files, function(name, size, type, datapath) {
      file.copy(datapath, to = file.path("www", "xlsx", name), overwrite = TRUE)
    })
  })
  
  xlsx_contents <- shiny::reactive({
   id <- shiny::showNotification(
      ui = "Data are loading, please wait ...", 
      duration = NULL, closeButton = FALSE
    )
    on.exit(shiny::removeNotification(id), add = TRUE)
    shiny::req(length(list.files("www/xlsx")) != 0 | !is.null(input[["xlsx_files"]]))
    get_xlsx_contents(
      path = "www/xlsx", 
      od_outlier = input[["od_outlier"]] %||% 1.5, 
      lm_outlier = input[["lm_outlier"]] %||% 1.5
    )
  })


  ## Blank tab -------------------------------------------------------------------------------------
  
  od_box_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = dplyr::bind_rows(shiny::req(xlsx_contents()), dplyr::mutate(shiny::req(xlsx_contents()), Step = "ALL")),
      mapping = ggplot2::aes(x = .data[["Step"]], y = .data[["re_OD"]], colour = .data[["Step"]])
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_rect(
        data = ~ .x %>% 
          dplyr::distinct(.data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")) %>%
          dplyr::mutate(
            ymin = ifelse(grepl("lower", .data[["name"]]), -Inf, .data[["value"]]),
            ymax = ifelse(grepl("upper", .data[["name"]]), Inf, .data[["value"]])
          ),
        mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
        fill = "firebrick2",
        alpha = 0.2,
        colour = "transparent",
        inherit.aes = FALSE,
        linetype = 2
      ) +
      ggplot2::geom_hline(
        data = ~ .x %>% 
          dplyr::distinct(.data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")),
        mapping = ggplot2::aes(yintercept = .data[["value"]]),
        colour = "firebrick2",
        linetype = 2
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.2) +
      ggbeeswarm::geom_beeswarm(
        data = ~ dplyr::filter(.x, .data[["is_outlier"]]), 
        colour = "firebrick2",
        size = input[["point_size"]]
      ) +
      ggbeeswarm::geom_quasirandom(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]), 
        shape = 1,
        size = input[["point_size"]]
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = NULL,
        y = bquote("Relative Error:"~frac((OD[2] - OD[1]), OD[1]))
      ) +
      ggplot2::theme(legend.position = "none")
  })

  od_density_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()),
      mapping = ggplot2::aes(x = .data[["re_OD"]], colour = .data[["Step"]], fill = .data[["Step"]])
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_rect(
        data = ~ .x %>% 
          dplyr::distinct(.data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")) %>%
          dplyr::mutate(
            xmin = ifelse(grepl("lower", .data[["name"]]), -Inf, .data[["value"]]),
            xmax = ifelse(grepl("upper", .data[["name"]]), Inf, .data[["value"]])
          ),
        mapping = ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]], ymin = -Inf, ymax = Inf),
        fill = "firebrick2",
        alpha = 0.2,
        colour = "transparent",
        inherit.aes = FALSE,
        linetype = 2
      ) +
      ggplot2::geom_vline(
        data = ~ .x %>% 
          dplyr::distinct(.data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")),
        mapping = ggplot2::aes(xintercept = .data[["value"]]),
        colour = "firebrick2",
        linetype = 2
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::geom_density(alpha = 0.2) +
      ggplot2::geom_density(mapping = ggplot2::aes(colour = "ALL", fill = "ALL"), alpha = 0.2) +
      ggplot2::scale_x_continuous(labels = scales::percent, expand = ggplot2::expand_scale(mult = c(0, 0))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = bquote("Relative Error:"~frac((OD[2] - OD[1]), OD[1])),
        y = "Density",
        colour = NULL,
        fill = NULL
      )
  })
  
  od_lm_box_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::select("file", dplyr::matches("_Intercept|_Slope")) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(
          cols = -c("file"), 
          names_to = c(".value", "term"),
          names_pattern = "(.*)_([^_]*)",
        ),
      mapping = ggplot2::aes(x = .data[["term"]], y = .data[["estimate"]], group = .data[["term"]])
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_rect(
        data = ~ .x %>%
          dplyr::distinct(.data[["term"]], .data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")) %>%
          dplyr::mutate(
            ymin = ifelse(grepl("lower", .data[["name"]]), -Inf, .data[["value"]]),
            ymax = ifelse(grepl("upper", .data[["name"]]), Inf, .data[["value"]])
          ),
        mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
        fill = "firebrick2",
        alpha = 0.2,
        colour = "transparent",
        inherit.aes = FALSE,
        linetype = 2
      ) +
      ggplot2::geom_hline(
        data = ~ .x %>%
          dplyr::distinct(.data[["term"]], .data[["lower_threshold"]], .data[["upper_threshold"]]) %>%
          tidyr::pivot_longer(cols = c("lower_threshold", "upper_threshold")),
        mapping = ggplot2::aes(yintercept = .data[["value"]]),
        colour = "firebrick2",
        linetype = 2
      ) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.2) +
      ggbeeswarm::geom_beeswarm(
        data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
        colour = "firebrick2",
        size = input[["point_size"]]
      ) +
      ggbeeswarm::geom_quasirandom(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
        mapping = ggplot2::aes(colour = .data[["file"]]),
        shape = 1,
        size = input[["point_size"]]
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::labs(x = NULL, y = "Estimate") +
      ggplot2::facet_wrap(facets = ggplot2::vars(.data[["term"]]), scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
  })
  
  od_lm_line_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::filter(.data[["Step"]] == "BLANK" & .data[["Concentration (µg/L)"]] != 0),
      mapping = ggplot2::aes(
        x = .data[["Concentration (µg/L)"]], 
        y = .data[["normalised_OD"]], 
        colour = .data[["file"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, .data[["is_outlier"]]), 
        colour = "firebrick2", 
        size = input[["point_size"]]
      ) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
        shape = 1,
        size = input[["point_size"]]
      ) +
      ggplot2::geom_smooth(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]), 
        method = "lm", se = FALSE
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        x = "Concentration (µg/L)",
        y = "Normalised Optical Density (OD)"
      ) +
      ggplot2::theme(legend.position = "none")
  })
  
  
  ## Fold-Change tab -------------------------------------------------------------------------------
  
  fold_change_plot <- shiny::reactive({ 
    fc_threshold <- input[["fold_change"]]
    ggplot2::ggplot(
      data = xlsx_contents() %>% 
        dplyr::select("file", "sheet_name", "Step", "Target", "Type", "fc_sn2_sn1", "is_any_outlier") %>% 
        tidyr::drop_na(),
      mapping = ggplot2::aes(x = .data[["fc_sn2_sn1"]], colour = .data[["Type"]], fill = .data[["Type"]])
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_rect(
        data = dplyr::tibble(
          xmax = fc_threshold, 
          is_any_outlier = rep(c(FALSE, TRUE), each = 2), 
          Type = rep(c("Control", "Target"), times = 2)
        ),
        mapping = ggplot2::aes(xmin = -Inf, xmax = .data[["xmax"]], ymin = -Inf, ymax = Inf),
        fill = "firebrick2",
        alpha = 0.2,
        colour = "transparent",
        inherit.aes = FALSE,
        linetype = 2
      ) +
      ggplot2::geom_vline(
        data = dplyr::tibble(
          xintercept = fc_threshold, 
          is_any_outlier = rep(c(FALSE, TRUE), each = 2), 
          Type = rep(c("Control", "Target"), times = 2)
        ),
        mapping = ggplot2::aes(xintercept = xintercept), 
        colour = "firebrick2", 
        linetype = 2
      ) +
      ggplot2::geom_density(alpha = 0.2) +
      ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(mult = c(0, 0))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = bquote("Insulin Secretion"~group("(", log[2]("SN2"/"SN1"), ")")),
        y = "Density",
        colour = NULL,
        fill = NULL
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["Type"]]),
        cols = ggplot2::vars(.data[["is_any_outlier"]]), 
        labeller = ggplot2::labeller(
          .cols = function(.x) c("FALSE" = "Good Values", "TRUE" = "Possibly Biased Values")[.x]
        )
      ) +
      ggplot2::theme(legend.position = "none")
  })
  
  
  
  ## Analysis tab ----------------------------------------------------------------------------------
  output$sheet_ui <- shiny::renderUI({ })
  
  output$experiment_ui <- shiny::renderUI({ })
  
  blank_plot <- shiny::reactive({ 
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::filter(.data[["Step"]] == "BLANK" & .data[["Concentration (µg/L)"]] != 0) %>% 
        dplyr::filter(!.data[["is_any_outlier"]]),
      mapping = ggplot2::aes(
        x = .data[["Concentration (µg/L)"]], 
        y = .data[["normalised_OD"]], 
        colour = .data[["file"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, .data[["is_outlier"]]), 
        colour = "firebrick2", 
        size = input[["point_size"]]
      ) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
        shape = 1,
        size = input[["point_size"]]
      ) +
      ggplot2::geom_smooth(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]), 
        method = "lm", se = FALSE
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        x = "Concentration (µg/L)",
        y = "Normalised Optical Density (OD)"
      ) +
      ggplot2::theme(legend.position = "none")
  })

  is_od_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::filter(!.data[["is_any_outlier"]]) %>% 
        tidyr::drop_na(.data[["Type"]], .data[["Target"]]),
      mapping = ggplot2::aes(
        x = .data[["Sample"]], 
        y = .data[["normalised_OD"]], 
        colour = paste0(.data[["Type"]], ": ", .data[["Target"]]), 
        fill = paste0(.data[["Type"]], ": ", .data[["Target"]]),
        group = paste0(.data[["Type"]], ": ", .data[["Target"]])
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_errorbar(
        stat = "summary",
        fun.data = "mean_se",
        position = position_dodge(width = 0.9),
        width = 0.25, 
        show.legend = FALSE, 
        na.rm = TRUE
      ) +
      ggplot2::geom_bar(stat = "summary", fun.y = mean, position = position_dodge(width = 0.9)) +
      ggplot2::geom_label(
        stat = "summary", 
        fun.data = function(x) {
          x <- stats::na.omit(x)
          data.frame(y = mean(x)/2, label = length(x))
        }, 
        position = position_dodge(width = 0.9),
        fill = "white", 
        colour = "black",
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2::labs(
        x = NULL,
        y = "Normalised Optical Density\n(OD)",
        colour = NULL,
        fill = NULL
      )
  })

  is_percent_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::filter(
          !.data[["is_any_outlier"]],
          Step %in% c("SN1", "SN2")
        ) %>% 
        tidyr::drop_na(.data[["Type"]], .data[["Target"]]),
      mapping = ggplot2::aes(
        x = .data[["Sample"]], 
        y = .data[["Insulin Secretion (% of content)"]],
        colour = paste0(.data[["Type"]], ": ", .data[["Target"]]), 
        fill = paste0(.data[["Type"]], ": ", .data[["Target"]])
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_boxplot(
        outlier.shape = NA, 
        alpha = 0.2, 
        position = position_dodge(width = 0.9),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        shape = 1,
        size = input[["point_size"]], 
        position = position_dodge(width = 0.9),
        na.rm = TRUE
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        limits = c(0, NA), 
        expand = ggplot2::expand_scale(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Insulin Secretion\n(% of content)",
        colour = NULL,
        fill = NULL
      )
  })

  is_logratio_plot <- shiny::reactive({
    ggplot2::ggplot(
      data = shiny::req(xlsx_contents()) %>% 
        dplyr::filter(!.data[["is_any_outlier"]]) %>% 
        tidyr::drop_na(.data[["Type"]], .data[["Target"]]) %>% 
        dplyr::distinct(.keep_all = TRUE,
          .data[["file"]], .data[["sheet_name"]], .data[["Condition"]], .data[["Type"]],
          .data[["Target"]]
        ),
      mapping = ggplot2::aes(
        x = .data[["Condition"]], 
        y = .data[["fc_sn2_sn1"]],
        colour = paste0(.data[["Type"]], ": ", .data[["Target"]]), 
        fill = paste0(.data[["Type"]], ": ", .data[["Target"]]),
        group = paste0(.data[["Type"]], ": ", .data[["Target"]])
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::geom_errorbar(
        stat = "summary",
        fun.data = "mean_se",
        position = position_dodge(width = 0.9),
        width = 0.25, 
        show.legend = FALSE, 
        na.rm = TRUE
      ) +
      ggplot2::geom_bar(stat = "summary", fun.y = mean, position = position_dodge(width = 0.9)) +
      ggplot2::geom_label(
        stat = "summary", 
        fun.data = function(x) {
          x <- stats::na.omit(x)
          data.frame(y = mean(x)/2, label = length(x))
        }, 
        position = position_dodge(width = 0.9),
        fill = "white", 
        colour = "black",
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expand_scale(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        x = NULL,
        y = bquote(atop("Insulin Secretion", group("(", log[2]("SN2"/"SN1"), ")"))),
        colour = NULL,
        fill = NULL
      )
  })
  
  is_plot <- shiny::reactive({
    ggpubr::ggarrange(
      is_od_plot(), is_percent_plot(), #is_logratio_plot(), 
      nrow = 1, ncol = 2, align = "v", 
      legend = "top", common.legend = TRUE
    )
  })
  
  is_foldchange_plot <- shiny::reactive({
    is_logratio_plot()
  })
}

shinyApp(ui, server)
