invisible(suppressPackageStartupMessages({
  sapply(
    X = c(
      "shiny", "DT", "rlang", "ggplot2", "scales", "purrr", 
      "dplyr", "tidyr", "glue", "readxl", "stats"
    ),
    FUN = library, character.only = TRUE
  )
}))

card <- function(title, body, type = "") {
  shiny::tags$div(class = paste("card", type),
    shiny::tags$div(class = "card-header", align = "center", title),
    shiny::tags$div(class = "card-body", align = "center", body)
  )
}

plotDownloadInputUI <- function(id, label, height = "330px") {
  ns <- shiny::NS(id)
  card(
    title = {
      shiny::tags$h4(label, align = "center",
        shiny::downloadButton(ns("plot_download"), label = "Download")
      )
    },
    body = { shiny::plotOutput(ns("plot"), height = height) }
  )
}

plotDownloadInput <- function(id, plot_object, width, height, dpi) {
  shiny::callModule(id = id, function(input, output, session) {
    
    output$plot <- shiny::renderPlot({ plot_object })
    
    output$plot_download <- shiny::downloadHandler(
      filename = function() { paste0(id, ".png") },
      content = function(file) {
        ggplot2::ggsave(
          filename = file, plot = plot_object,
          width = width, 
          height = height,
          units = "cm",
          dpi = dpi
        )
      }
    )
  })
}

get_xlsx_contents <- function(files, project_name = NULL, od_outlier = 1.5, lm_outlier = 1.5) {
  out_all_excel <- dplyr::tibble(file = files, filename = basename(file)) %>%
    dplyr::mutate(sheet_name = purrr::map(.data[["file"]], readxl::excel_sheets)) %>%
    tidyr::unnest(.data[["sheet_name"]]) %>%
    dplyr::mutate(
      nested_data = purrr::map2(.data[["file"]], .data[["sheet_name"]], function(.x, .y) {
        tmp <- suppressMessages({readxl::read_xlsx(.x, .y)})
        dplyr::mutate(tmp[, 1:max(grep("Volume \\(µl\\)|OD2", colnames(tmp)))],
          re_OD = (.data[["OD2"]] - .data[["OD1"]]) / .data[["OD1"]],
          mean_OD = purrr::map2_dbl(.data[["OD1"]], .data[["OD2"]], ~ mean(c(.x, .y)))
        )
      })
    ) %>%
    tidyr::unnest(nested_data)
  
  if (is.null(project_name)) project_name <- unique(out_all_excel[["Project"]])
  
  out_excel <- out_all_excel %>%
    dplyr::filter(.data[["Project"]] %in% !!project_name) %>% 
    dplyr::mutate(
      lower_threshold = stats::quantile(.data[["re_OD"]], 0.25) - od_outlier * stats::IQR(.data[["re_OD"]]),
      upper_threshold = stats::quantile(.data[["re_OD"]], 0.75) + od_outlier * stats::IQR(.data[["re_OD"]]),
      is_outlier_OD = .data[["re_OD"]] < .data[["lower_threshold"]] | .data[["re_OD"]] > .data[["upper_threshold"]]
    ) %>%
    dplyr::group_by(.data[["filename"]]) %>%
    dplyr::mutate(normalised_OD = .data[["mean_OD"]] - .data[["mean_OD"]][1]) %>%
    dplyr::ungroup()
  
  out_model <- out_excel %>% 
    dplyr::filter(
      .data[["Step"]] == "BLANK",
      .data[["Concentration (mU/L)"]] != 0,
      !.data[["is_outlier_OD"]]
    ) %>% 
    dplyr::group_by(.data[["filename"]]) %>% 
    dplyr::do({
      lm = broom::tidy(
        stats::lm(
          formula = log10(normalised_OD) ~ log10(`Concentration (mU/L)` / 23),
          data = .
        )
      )[, c("term", "estimate")]
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      term = c("(Intercept)" = "Intercept", "log10(`Concentration (mU/L)`/23)" = "Slope")[.data[["term"]]]
    ) %>% 
    dplyr::group_by(.data[["term"]]) %>% 
    dplyr::mutate(
      lower_threshold = stats::quantile(.data[["estimate"]], 0.25) - lm_outlier * stats::IQR(.data[["estimate"]]),
      upper_threshold = stats::quantile(.data[["estimate"]], 0.75) + lm_outlier * stats::IQR(.data[["estimate"]]),
      is_outlier = .data[["estimate"]] < .data[["lower_threshold"]] | .data[["estimate"]] > .data[["upper_threshold"]]
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(
      id_cols = "filename", 
      names_from = "term", 
      values_from = c("estimate", "lower_threshold", "upper_threshold", "is_outlier")
    )
  
  out_excel_model <- dplyr::full_join(x = out_excel, y = out_model, by = "filename") %>% 
    dplyr::group_by(.data[["filename"]]) %>% 
    dplyr::mutate(
      "Concentration (µg/L)" = .data[["Dilution Factor"]] *
        10^((log10(.data[["normalised_OD"]]) - .data[["estimate_Intercept"]]) / .data[["estimate_Slope"]]),
      "Concentration (µg/L)" = ifelse(
        test = is.na(.data[["Concentration (µg/L)"]]) & .data[["Step"]] == "BLANK",
        yes =  .data[["Concentration (mU/L)"]] / 23,
        no = .data[["Concentration (µg/L)"]]
      ),
      "Total (ng)" = .data[["Volume (µl)"]] / 10^3 * .data[["Concentration (µg/L)"]]
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]], .data[["Target"]], .data[["Step"]]) %>% 
    dplyr::mutate(measure_id = 1:dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      is_any_outlier = .data[["is_outlier_OD"]] | 
        .data[["is_outlier_Intercept"]] | 
        .data[["is_outlier_Slope"]]
    )
  
  out_insulin_tmp <- out_excel_model %>% 
    dplyr::filter(!.data[["is_any_outlier"]]) %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]], .data[["Target"]]) %>% 
    tidyr::pivot_wider(
      id_cols = c("filename", "sheet_name", "Target", "measure_id"), 
      names_from = "Step", 
      values_from = "Total (ng)"
    ) %>% 
    dplyr::mutate(
      ins_SN1 = .data[["SN1"]] / (.data[["LYSAT"]] + .data[["SN1"]] + .data[["SN2"]]),
      ins_SN2 = .data[["SN2"]] / (.data[["LYSAT"]] + .data[["SN2"]])
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select("filename", "sheet_name", "Target", "measure_id", tidyr::num_range("ins_SN", 1:2))
  
  out_insulin <- out_insulin_tmp %>% 
    tidyr::pivot_longer(
     cols = tidyr::num_range("ins_SN", 1:2), 
     names_to = "Step", 
     names_pattern = "ins_(.*)",
     values_to = "Insulin Secretion (% of content)"
    ) %>% 
    tidyr::drop_na("Insulin Secretion (% of content)")

  out_excel_insulin <- dplyr::full_join(
    x = out_excel_model, 
    y = out_insulin, 
    by = c("filename", "sheet_name", "Step", "Target", "measure_id")
  )
  
  out_insulin_fc <- out_insulin_tmp %>% 
    tidyr::drop_na(tidyr::num_range("ins_SN", 1:2)) %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]], .data[["Target"]], .data[["measure_id"]]) %>% 
    dplyr::transmute(
      "fc_sn2_sn1" = ins_SN2 / ins_SN1,
      "log2_fc_sn2_sn1" = log2(ins_SN2 / ins_SN1)
    ) %>% 
    dplyr::ungroup()
  
  dplyr::full_join(
    x = out_excel_insulin, 
    y = out_insulin_fc, 
    by = c("filename", "sheet_name", "Target", "measure_id")
  ) %>% 
    dplyr::mutate(
      is_any_outlier = .data[["is_outlier_OD"]] | 
        .data[["is_outlier_Intercept"]] | 
        .data[["is_outlier_Slope"]] |
        (is.na(.data[["fc_sn2_sn1"]]) & !.data[["Step"]] %in% "BLANK") |
        (is.na(.data[["log2_fc_sn2_sn1"]]) & !.data[["Step"]] %in% "BLANK") |
        (is.na(.data[["Insulin Secretion (% of content)"]]) & !.data[["Step"]] %in% "BLANK"),
      Sample = gsub("mM ", "mM\n", .data[["Sample"]])
    ) %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]]) %>% 
    dplyr::arrange(.data[["Condition"]], .data[["Sample"]]) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      Sample = factor(Sample, levels = unique(.data[["Sample"]])),
      Type = factor(Type, levels = c("Reference", "Control", "Target")),
    ) %>% 
    dplyr::arrange(.data[["Type"]], .data[["Target"]]) %>% 
    dplyr::mutate(
      Type_Target = factor(
        x = paste0(.data[["Type"]], ": ", .data[["Target"]]), 
        levels = unique(paste0(.data[["Type"]], ": ", .data[["Target"]]))
      )
    )
}

dir.create(file.path("www", "xlsx"), showWarnings = FALSE, mode = "0777")

# UI-side ==========================================================================================
ui <- shiny::navbarPage(
  theme = "yeti-bootstrap.min.css",
  title = "EndoC-βH1",
  windowTitle = "EndoC-βH1",
  collapsible = TRUE,
  id = "main-menu",
  selected = "upload-tab",
  ## Upload tab ------------------------------------------------------------------------------------
  shiny::tabPanel("Upload Experiments & Plot Settings", icon = shiny::icon("file-upload"), value = "upload-tab",
    shiny::fluidRow(
      shiny::column(width = 3, align = "center",
        card(title = "Plot Settings", body = {
          shiny::tagList(
            shiny::numericInput("font_size", shiny::tags$span("Font Size", shiny::helpText("(pt)")),
              value = 16
            ),
            shiny::numericInput("point_size", shiny::tags$span("Point Size", shiny::helpText("(mm)")),
              value = 2,
              min = 0, max = 4, step = 0.5
            ),
            shiny::numericInput("plot_width", shiny::tags$span("Width", shiny::helpText("(cm)")),
              value = 16
            ),
            shiny::numericInput("plot_height", shiny::tags$span("Height", shiny::helpText("(cm)")),
              value = 12
            ),
            shiny::numericInput("plot_dpi", shiny::tags$span("DPI", shiny::helpText("(Default: 120)")),
              value = 120
            )
          )
        })
      ),
      shiny::column(width = 9, align = "center",
        shiny::fluidRow(
          shiny::column(width = 6, card(title = "Project", body = shiny::uiOutput("project_ui"))),
          shiny::column(width = 6,
            card(title = "Upload Experiments in Excel Files", body = {
              shiny::fileInput("xlsx_files", 
                shiny::tags$span("Choose One or Several Excel Files", 
                  shiny::helpText(shiny::downloadLink("template", "(Use only the following template!)"))
                ), 
                multiple = TRUE, width = "90%",
                accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
              )
            })
          )
        ),
        shiny::uiOutput("upload_ui")
      )
    )
  ),
  ## Blank tab -------------------------------------------------------------------------------------
  shiny::tabPanel("Blank Quality-Control", icon = shiny::icon("chart-line"), value = "blank-tab", 
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3, 
        shiny::tags$div(align = "center", 
          shiny::sliderInput("od_outlier", 
            shiny::tags$span("Optical Density (OD) Outliers Threshold", 
              shiny::helpText(
                "(Higher than", shiny::tags$strong("X"), 
                "times the interquartile range above the 75", shiny::tags$sup("th", .noWS = "before"), "percentile",
                "or",
                "lower than", shiny::tags$strong("X"), 
                "times the interquartile range below the 25", shiny::tags$sup("th", .noWS = "before"), "percentile)"
              )
            ),
            min = 1, max = 5, value = 3, step = 0.25
          ), 
          shiny::sliderInput("lm_outlier", 
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
  ## Analysis tab ----------------------------------------------------------------------------------
  shiny::tabPanel("Insulin Secretion Analysis", icon = shiny::icon("chart-bar"), value = "is_analysis-tab", 
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3, 
        shiny::tags$div(align = "center", 
          shiny::numericInput("fold_change",
            shiny::tags$span('Threshold to Define "Secretion"',
              shiny::helpText(
                "(Threshold above which cells are allegedly secreting insulin)"
              )
            ),
            value = 1, step = 0.1
          ),
          shiny::radioButtons("search_by", "Search by", 
            choices = c("File", "Target"), selected = "File", 
            inline = TRUE
          ), 
          shiny::uiOutput("target_ui"),
          shiny::uiOutput("experiment_ui")
        )
      ), 
      shiny::mainPanel(width = 9,
        shiny::fluidRow(
          shiny::column(width = 5, align = "center",
            plotDownloadInputUI("is_ratio_distribution_plot", "Insulin Secretion (SN2/SN1) Distribution")
          ),
          shiny::column(width = 7, align = "center",
            plotDownloadInputUI("is_foldchange_plot", "Insulin Secretion (SN2/SN1)")
          )
        ),
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 12, align = "center",
            plotDownloadInputUI("is_plot", "Insulin Secretion")
          )
        )
      )
    )
  ),
  ## Outliers tab ----------------------------------------------------------------------------------
  shiny::tabPanel(title = "Outliers", value = "outliers-tab",
    card(title = shiny::tags$h4("Outliers List"), DT::dataTableOutput("outliers"))
  )
)

# Server-side ======================================================================================
server <- function(input, output, session) {
  shiny::observe({
    shiny::req(input[["plot_dpi"]], input[["plot_height"]], input[["plot_dpi"]])
    purrr::map2(
      .x = list(
        "od_box_plot", "od_density_plot", "od_lm_box_plot", "od_lm_line_plot"
      ),
      .y = list(
        od_box_plot(), od_density_plot(), od_lm_box_plot(), od_lm_line_plot()
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

  shiny::observe({
    shiny::req(input[["plot_dpi"]], input[["plot_height"]], input[["plot_dpi"]])
    purrr::map2(
      .x = list(
        "is_ratio_distribution_plot",
        "is_od_plot", "is_percent_plot", "is_foldchange_plot",
        "is_plot", "is_logratio_plot"
      ),
      .y = list(
        is_ratio_distribution_plot(),
        is_od_plot(), is_percent_plot(), is_foldchange_plot(),
        is_plot(), is_logratio_plot()
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
  output$template <- shiny::downloadHandler(
    filename = function() "endoc-bh1_template.xlsx",
    content = function(file) file.copy(file.path("www", "template.xlsx"), file, overwrite = TRUE)
  )
  
  output$xlsx_contents_summary <- shiny::renderUI({
    xlsx_size <- sum(input$xlsx_files[, "size"])
    class(xlsx_size) <- "object_size"
    shiny::tags$p(
      "A total of", shiny::tags$strong(length(input$xlsx_files[, "name"])), 
      if (nrow(input$xlsx_files) > 1) {
        "Excel files were succesfully uploaded,"
      } else {
        "Excel file was succesfully uploaded,"
      },
      "for a total amount of", shiny::tags$strong(format(xlsx_size, units = "Kb"), .noWS = "after"), "."
    )
  })
  
  xlsx_available <- shiny::reactive({
    input$xlsx_files
    
    dplyr::tibble(file = list.files(path = "www/xlsx", pattern = ".xlsx$", full.names = TRUE)) %>%
      dplyr::mutate(sheet_name = purrr::map(.data[["file"]], readxl::excel_sheets)) %>%
      tidyr::unnest(.data[["sheet_name"]]) %>%
      dplyr::mutate(
        Project = purrr::map2_chr(.data[["file"]], .data[["sheet_name"]], function(.x, .y) {
          suppressMessages({
            unique(readxl::read_xlsx(.x, .y)[["Project"]])
          })
        }),
        filename = basename(.data[["file"]])
      ) %>% 
      dplyr::group_by(.data[["Project"]], .data[["filename"]], .data[["file"]]) %>% 
      dplyr::summarise_at(
        .vars = dplyr::vars(.data[["sheet_name"]]), 
        .funs = ~ glue::glue_collapse(.x, sep = ", ", last = " and ")
      ) %>% 
      dplyr::ungroup()
  })
  
  output$xlsx_available <- DT::renderDataTable({ 
    dplyr::select(shiny::req(xlsx_available()), 
      c("Project", "File" = "filename", "Sheet" = "sheet_name")
    ) 
  })
  
  output$project_ui <- shiny::renderUI({
    shiny::selectInput("project_name", "Available Projects",
      choices = unique(xlsx_available()[["Project"]]),
      selected = isolate(input[["project_name"]]),
      width = "90%",
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$upload_ui <- shiny::renderUI({
    list(
      if (!is.null(input$xlsx_files)) {
        list(
          shiny::fluidRow(style = "padding-top: 1em;",
            shiny::column(width = 12,
              card(title = "Excel Files Uploaded", body = shiny::uiOutput("xlsx_contents_summary"))
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

  xlsx_contents <- shiny::reactive({
    shiny::req(input[["project_name"]])
    id <- shiny::showNotification(
      ui = "Data are loading, please wait ...", 
      duration = NULL, closeButton = FALSE
    )
    on.exit(shiny::removeNotification(id), add = TRUE)
    if (!is.null(input$xlsx_files)) {
      purrr::pmap(shiny::req(input$xlsx_files), function(name, size, type, datapath) {
        file.copy(datapath, to = file.path("www", "xlsx", name), overwrite = TRUE)
      })
    }
    if (length(list.files(file.path("www", "xlsx"), pattern = ".xlsx$")) != 0) {
      get_xlsx_contents(
        files = xlsx_available()[["file"]],
        od_outlier = input[["od_outlier"]] %||% 3, 
        lm_outlier = input[["lm_outlier"]] %||% 1.5,
        project_name = input[["project_name"]] %||% NULL
      )
    }
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
        data = ~ dplyr::filter(.x, .data[["is_outlier_OD"]]), 
        colour = "firebrick2",
        size = input[["point_size"]],
        groupOnX = TRUE
      ) +
      ggbeeswarm::geom_quasirandom(
        data = ~ dplyr::filter(.x, !.data[["is_outlier_OD"]]), 
        shape = 1,
        size = input[["point_size"]],
        groupOnX = TRUE
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
        dplyr::select("filename", dplyr::matches("_Intercept|_Slope")) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(
          cols = -c("filename"), 
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
        size = input[["point_size"]],
        groupOnX = TRUE
      ) +
      ggbeeswarm::geom_quasirandom(
        data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
        mapping = ggplot2::aes(colour = .data[["filename"]]),
        shape = 1,
        size = input[["point_size"]],
        groupOnX = TRUE
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
        colour = .data[["filename"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, !(.data[["is_outlier_OD"]] | .data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]])),
        shape = 1,
        size = input[["point_size"]]
      ) +
      ggplot2::geom_smooth(
        data = ~ dplyr::filter(.x, !(.data[["is_outlier_OD"]] | .data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]])),
        method = "lm", se = FALSE
      ) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, (.data[["is_outlier_OD"]] | .data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]])),
        colour = "firebrick2", 
        size = input[["point_size"]]
      ) +
      ggplot2::geom_smooth(
        data = ~ dplyr::filter(.x, (.data[["is_outlier_OD"]] | .data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]])),
        colour = "firebrick2", 
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
  
  
  ## Analysis tab ----------------------------------------------------------------------------------
  output$target_ui <- shiny::renderUI({
    column_to_select <- c("File" = "filename", "Target" = "Target")[shiny::req(input[["search_by"]])]
    
    xlsx_contents_subset <- dplyr::filter(shiny::req(xlsx_contents()), Step != "BLANK")
    
    shiny::selectInput("targets_list", paste0("Available ", input[["search_by"]], "s"),
      choices = unique(xlsx_contents_subset[[column_to_select]]),
      selected = if (input[["search_by"]] == "File") unique(xlsx_contents_subset[[column_to_select]]) else NULL,
      width = "100%",
      selectize = FALSE,
      multiple = TRUE,
      size = 10
    )
  })
  
  output$experiment_ui <- shiny::renderUI({ 
    column_to_select <- c("File" = "filename", "Target" = "Target")[shiny::req(input[["search_by"]])]
    
    experiments_list <- shiny::req(xlsx_contents()) %>% 
      dplyr::mutate(experiment_file = glue::glue("{Target} ({filename})")) %>% 
      dplyr::filter(
        .data[[column_to_select]] %in% shiny::req(input[["targets_list"]]), 
        !.data[["Step"]] %in% "BLANK"
      )
      
    shiny::selectInput("experiments_list", "Available Experiments",
      choices = unique(experiments_list[["experiment_file"]]),
      selected = unique(experiments_list[["experiment_file"]]),
      width = "100%",
      selectize = FALSE,
      multiple = TRUE,
      size = 10
    )
  })
  
  xlsx_contents_is <- shiny::reactive({
    file_selected <- gsub("^.*\\((.*)\\)$", "\\1", shiny::req(input[["experiments_list"]]))
    if (length(file_selected) > 0) {
      out <- xlsx_contents() %>%
        dplyr::mutate(experiment_file = glue::glue("{Target} ({filename})")) %>%
        dplyr::filter(
          (.data[["Step"]] %in% "BLANK" & .data[["filename"]] %in% !!file_selected) |
            (.data[["Type"]] %in% "Control" & .data[["filename"]] %in% !!file_selected) |
            .data[["experiment_file"]] %in% !!input[["experiments_list"]]
        ) %>% 
        dplyr::filter(!.data[["is_any_outlier"]])
    } else {
      out <- dplyr::filter(xlsx_contents(), !.data[["is_any_outlier"]])
    }
  })
  
  xlsx_contents_selected <- shiny::reactive({
    list_exp_with_is <- shiny::req(xlsx_contents_is()) %>% 
      dplyr::filter(Type %in% "Reference") %>% 
      dplyr::group_by(.data[["filename"]]) %>% 
      dplyr::summarise(
        is_reference_good = mean(.data[["fc_sn2_sn1"]], na.rm = TRUE) >= shiny::req(input[["fold_change"]])
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(.data[["is_reference_good"]])
  
    dplyr::filter(xlsx_contents_is(),
      .data[["filename"]] %in% !!list_exp_with_is[["filename"]]
    )
  })
  
  is_ratio_distribution_plot <- shiny::reactive({
    if (nrow(shiny::req(xlsx_contents_selected())) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_light(base_size = input[["font_size"]]) +
          ggplot2::labs(
            x = NULL,
            y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")"))),
            colour = NULL,
            fill = NULL
          ) +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5, label = "Not available!",
            angle = 30, size = 24, colour = "red", alpha = 0.25
          ) +
          ggplot2::theme(axis.text = ggplot2::element_blank())
      )
    }
    
    fc_threshold <- shiny::req(input[["fold_change"]])
    
    gg_data <- shiny::req(xlsx_contents_is()) %>% 
      dplyr::filter(
        .data[["Type"]] %in% "Reference",
        !.data[["is_any_outlier"]],
        .data[["Step"]] %in% "SN2"
      ) %>% 
      dplyr::select(c("filename", "Type_Target", "fc_sn2_sn1")) %>% 
      dplyr::mutate(facet_file = gsub(".xlsx$", "", gsub("_", "\n", .data[["filename"]])))
    
    ggplot2::ggplot(
      data = gg_data,
      mapping = ggplot2::aes(
        x = .data[["Type_Target"]],
        y = .data[["fc_sn2_sn1"]],
        colour = .data[["Type_Target"]],
        fill = .data[["Type_Target"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_rect(
        data = dplyr::tibble(ymax = fc_threshold),
        mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .data[["ymax"]]),
        fill = "firebrick2",
        alpha = 0.2,
        colour = "transparent",
        inherit.aes = FALSE,
        linetype = 2
      ) +
      ggplot2::geom_hline(
        data = dplyr::tibble(yintercept = fc_threshold),
        mapping = ggplot2::aes(yintercept = yintercept),
        colour = "firebrick2",
        linetype = 2
      ) +
      ggplot2::geom_hline(
        data = dplyr::tibble(yintercept = 1),
        mapping = ggplot2::aes(yintercept = yintercept),
        colour = "black",
        linetype = 3
      ) +
      ggplot2::geom_boxplot(alpha = 0.2, width = 0.5) +
      ggplot2::geom_violin(alpha = 0.2) +
      ggbeeswarm::geom_quasirandom(shape = 1) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2::scale_colour_viridis_d(end = 0.9) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = NULL,
        y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")"))),
        colour = NULL,
        fill = NULL
      ) +
      ggplot2::theme(legend.position = "none")
  })

  is_od_plot <- shiny::reactive({
    if (nrow(shiny::req(xlsx_contents_selected())) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_light(base_size = input[["font_size"]]) +
          ggplot2::labs(
            x = NULL,
            y = "Normalised Optical Density\n(OD)"
          ) +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5, label = "Not available!",
            angle = 30, size = 24, colour = "red", alpha = 0.25
          ) +
          ggplot2::theme(axis.text = ggplot2::element_blank())
      )
    }
    
    ggplot2::ggplot(
      data = tidyr::drop_na(xlsx_contents_selected(), .data[["Type"]], .data[["Target"]]),
      mapping = ggplot2::aes(
        x = .data[["Sample"]], 
        y = .data[["normalised_OD"]], 
        colour = .data[["Type_Target"]], 
        fill = .data[["Type_Target"]],
        group = .data[["Type_Target"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_errorbar(
        stat = "summary",
        fun.data = "mean_se",
        position = ggplot2::position_dodge(width = 0.9),
        width = 0.25, 
        show.legend = FALSE, 
        na.rm = TRUE
      ) +
      ggplot2::geom_bar(stat = "summary", fun.y = mean, position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::geom_label(
        stat = "summary", 
        fun.data = function(x) {
          x <- stats::na.omit(x)
          data.frame(y = mean(x)/2, label = length(x))
        }, 
        position = ggplot2::position_dodge(width = 0.9),
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
    if (nrow(shiny::req(xlsx_contents_selected())) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_light(base_size = input[["font_size"]]) +
          ggplot2::labs(
            x = NULL,
            y = "Insulin Secretion\n(% of content)",
            colour = NULL,
            fill = NULL
          ) +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5, label = "Not available!",
            angle = 30, size = 24, colour = "red", alpha = 0.25
          ) +
          ggplot2::theme(axis.text = ggplot2::element_blank())
      )
    }
    
    ggplot2::ggplot(
      data = xlsx_contents_selected() %>% 
        dplyr::filter(Step %in% c("SN1", "SN2")) %>% 
        tidyr::drop_na(.data[["Type"]], .data[["Target"]]) %>% 
        dplyr::select(c("Sample", "Type_Target", "Insulin Secretion (% of content)")) %>% 
        dplyr::distinct(),
      mapping = ggplot2::aes(
        x = .data[["Sample"]], 
        y = .data[["Insulin Secretion (% of content)"]],
        colour = .data[["Type_Target"]], 
        fill = .data[["Type_Target"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_boxplot(
        outlier.shape = NA, 
        alpha = 0.2, 
        position = ggplot2::position_dodge(width = 0.9),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        shape = 1,
        size = input[["point_size"]],
        position = ggplot2::position_jitterdodge(dodge.width = 0.9, jitter.width = 0.25),
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
    if (nrow(shiny::req(xlsx_contents_selected())) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_light(base_size = input[["font_size"]]) +
          ggplot2::labs(
            x = NULL,
            y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")")))
          ) +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5, label = "Not available!",
            angle = 30, size = 24, colour = "red", alpha = 0.25
          ) +
          ggplot2::theme(axis.text = ggplot2::element_blank())
      )
    }
    
    ggplot2::ggplot(
      data = xlsx_contents_selected() %>% 
        tidyr::drop_na(.data[["Type"]], .data[["Target"]]) %>%
        dplyr::select(c("Condition", "Type_Target", "fc_sn2_sn1")) %>% 
        dplyr::distinct(),
      mapping = ggplot2::aes(
        x = .data[["Condition"]], 
        y = .data[["fc_sn2_sn1"]],
        colour = .data[["Type_Target"]], 
        fill = .data[["Type_Target"]],
        group = .data[["Type_Target"]]
      )
    ) +
      ggplot2::theme_light(base_size = input[["font_size"]]) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::geom_errorbar(
        stat = "summary",
        fun.data = "mean_se",
        position = ggplot2::position_dodge(width = 0.9),
        width = 0.25, 
        show.legend = FALSE, 
        na.rm = TRUE
      ) +
      ggplot2::geom_bar(stat = "summary", fun.y = mean, position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::geom_label(
        stat = "summary", 
        fun.data = function(x) {
          x <- stats::na.omit(x)
          data.frame(y = mean(x)/2, label = length(x))
        }, 
        position = ggplot2::position_dodge(width = 0.9),
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
        y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")"))),
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
  
  
  ## Outliers tab ----------------------------------------------------------------------------------
  shiny::observe({
    if (nrow(outliers_list()) == 0) {
      shiny::hideTab("main-menu", target = "outliers-tab")
    } else {
      shiny::showTab("main-menu", target = "outliers-tab")
    }
  })
  
  outliers_list <- shiny::reactive({
    columns_outliers <- c(
      "filename", "Target", "Type", "Step", "Condition", 
      'Blank "Intercept" outlier' = "is_outlier_Intercept", 
      'Blank "Slope" outlier' = "is_outlier_Slope", 
      "OD outlier" = "is_outlier_OD",
      "Excluded" = "is_any_outlier"
    )
    shiny::req(xlsx_contents()) %>% 
      dplyr::filter(.data[["is_any_outlier"]]) %>% 
      dplyr::select(!!columns_outliers) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(.data[["filename"]], .data[["Type"]], .data[["Target"]], .data[["Condition"]])
  })
  
  output$outliers <- DT::renderDataTable({ outliers_list() })
}

shiny::shinyApp(ui, server)
