dir.create(file.path("www", "xlsx"), showWarnings = FALSE, mode = "0777")

invisible(suppressPackageStartupMessages({
  sapply(
    X = c(
      "shiny", "DT", "rlang", "ggplot2", "scales", "purrr", 
      "dplyr", "tidyr", "glue", "readxl", "stats", "broom",
      "ggbeeswarm", "ggpubr", "ggthemes"
    ),
    FUN = library, character.only = TRUE
  )
}))

ggplot2_themes <- c(
  paste0("ggplot2::", grep("theme_", ls("package:ggplot2"), value = TRUE), "()"),
  paste0("ggpubr::", grep("theme_", ls("package:ggpubr"), value = TRUE), "()"),
  paste0("ggthemes::", grep("theme_", ls("package:ggthemes"), value = TRUE), "()")
) %>% 
  setdiff(paste0("ggplot2::theme_", c("get", "set", "replace", "update"), "()")) %>% 
  setdiff(paste0("ggpubr::theme_", c("cleveland"), "()")) %>% 
  purrr::set_names(., gsub(".*::theme_(.*)\\(\\)", "\\1", .)) %>% 
  gsub("\\(\\)", "", .)

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
        (is.na(.data[["fc_sn2_sn1"]]) & !.data[["Step"]] %in% c("BLANK", "LYSAT")) |
        (is.na(.data[["Insulin Secretion (% of content)"]]) & !.data[["Step"]] %in% c("BLANK", "LYSAT")),
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
      Type_Target = gsub("NA: NA", NA, paste0(.data[["Type"]], ": ", .data[["Target"]])),
      Type_Target = factor(x = Type_Target, levels = unique(Type_Target))
    )
}

get_outliers <- function(data, fold_change) {
  if (!"is_reference_good" %in% colnames(data)) {
    data <- dplyr::full_join(
      x = data,
      y = data %>% 
        dplyr::filter(Type %in% "Reference") %>% 
        dplyr::group_by(.data[["filename"]]) %>% 
        dplyr::summarise(
          is_reference_good = mean(.data[["fc_sn2_sn1"]], na.rm = TRUE) >= fold_change
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(c("filename", "is_reference_good")),
      by = "filename"
    )
  }
  data %>% 
    dplyr::filter(.data[["is_any_outlier"]] | !.data[["is_reference_good"]]) %>% 
    dplyr::select(c(
      "filename", "Target", "Condition", 
      "is_outlier_Intercept", "is_outlier_Slope", 
      "is_outlier_OD",
      "is_reference_good"
    )) %>% 
    dplyr::distinct() %>% 
    tidyr::replace_na(list(Target = "BLANK")) %>% 
    dplyr::group_by(.data[["filename"]]) %>% 
    dplyr::summarise(
      "Blank Linearity" = any(.data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]]),
      "Technical Variability (OD)" = any(.data[["is_outlier_OD"]]),
      "Not Secreting Insulin in Reference" = any(!.data[["is_reference_good"]])
    )
}

# UI-side ==========================================================================================
ui <- shiny::navbarPage(
  theme = "yeti-bootstrap.min.css",
  title = "EndoC-βH1",
  windowTitle = "EndoC-βH1",
  collapsible = TRUE,
  id = "main_menu",
  selected = "upload-tab",
  ## Upload tab ------------------------------------------------------------------------------------
  shiny::tabPanel("Upload Experiments & Plot Settings", icon = shiny::icon("file-upload"), value = "upload-tab",
    shiny::fluidRow(
      shiny::column(width = 3, align = "center",
        card(title = "Plot Settings", body = {
          shiny::tagList(
            shiny::selectInput("ggplot2_theme", "Theme", 
              choices = ggplot2_themes[sort(names(ggplot2_themes))], 
              selected = grep("light", ggplot2_themes, value = TRUE)
            ),
            shiny::radioButtons("colour_scale", shiny::tags$span("Colour Palette", shiny::helpText('(No effect on "thresholds areas")')), 
              choices = c("Viridis", "Plasma", "Magma", "Inferno", "Grey"),
              inline = TRUE
            ),
            shiny::sliderInput("colour_scale_range", shiny::tags$span("Limits of the Colour Palette", shiny::helpText("(Dark to Bright)")), 
              min = 0, max = 1, 
              value = c(0, 0.85), 
              step = 0.05
            ),
            shiny::fluidRow(
              shiny::column(width = 6,
                shiny::numericInput("font_size", shiny::tags$span("Font Size", shiny::helpText("(pt)")),
                  value = 16
                )
              ),
              shiny::column(width = 6,
                shiny::numericInput("point_size", shiny::tags$span("Point Size", shiny::helpText("(mm)")),
                  value = 2,
                  min = 0, max = 4, step = 0.5
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(width = 6,
                shiny::numericInput("plot_width", shiny::tags$span("Width", shiny::helpText("(cm)")),
                  value = 16
                )
              ),
              shiny::column(width = 6,
                shiny::numericInput("plot_height", shiny::tags$span("Height", shiny::helpText("(cm)")),
                  value = 12
                )
              )
            ),
            shiny::numericInput("plot_dpi", shiny::tags$span("Resolution", shiny::helpText("(ppi)")),
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
  shiny::tabPanel("Technical Quality-Control", icon = shiny::icon("chart-line"), value = "blank-tab", 
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
          shiny::actionButton("show_issues", "Show Issues in the Selected Experiments"),
          shiny::tags$hr(),
          shiny::numericInput("fold_change",
            shiny::tags$span('Threshold to Define "Secretion"',
              shiny::helpText(
                "(Threshold above which cells are allegedly secreting insulin)"
              )
            ),
            value = 1, step = 0.1
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
            plotDownloadInputUI("is_ratio_plot", "Insulin Secretion (SN2/SN1)")
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
    shiny::req(
      input[["plot_dpi"]], input[["plot_height"]], input[["plot_dpi"]],
      od_box_plot(), od_density_plot(), od_lm_box_plot(), od_lm_line_plot()
    )
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
    shiny::req(
      input[["targets_list"]], input[["experiments_list"]],
      input[["plot_dpi"]], input[["plot_height"]], input[["plot_dpi"]],
      input[["fold_change"]], input[["font_size"]],
      is_ratio_distribution_plot(), is_plot(), is_ratio_plot()
    )
    purrr::map2(
      .x = list("is_ratio_distribution_plot", "is_plot", "is_ratio_plot"),
      .y = list(is_ratio_distribution_plot(), is_plot(), is_ratio_plot()),
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
    if (length(xlsx_available()[["file"]]) == 0) {
      purrr::map(
        .x = c("blank", "is_analysis", "outliers"),
        .f = ~ shiny::hideTab("main_menu", target = paste0(.x, "-tab"))
      )
    } else {
      purrr::map(
        .x = c("blank", "is_analysis"),
        .f = ~ shiny::showTab("main_menu", target = paste0(.x, "-tab"))
      )
      if (nrow(outliers_list()) == 0) {
        shiny::hideTab("main_menu", target = "outliers-tab")
      } else {
        shiny::showTab("main_menu", target = "outliers-tab")
      }
    }
  })

  ggplot2_theme <- shiny::reactive({ 
    package_theme <- strsplit(input[["ggplot2_theme"]], "::")[[1]]
    f <- utils::getFromNamespace(package_theme[2], package_theme[1]) 
    f(base_size = input[["font_size"]])
  })
  
  ggplot2_colour <- shiny::reactive({
    if (input[["colour_scale"]] == "Grey") {
      ggplot2::scale_colour_grey(
        start = input[["colour_scale_range"]][1], 
        end = input[["colour_scale_range"]][2]
      )
    } else {
      ggplot2::scale_colour_viridis_d(
        option = tolower(input[["colour_scale"]]), 
        begin = input[["colour_scale_range"]][1], 
        end = input[["colour_scale_range"]][2]
      )
    }
  })

  ggplot2_fill <- shiny::reactive({
    if (input[["colour_scale"]] == "Grey") {
      ggplot2::scale_fill_grey(
        start = input[["colour_scale_range"]][1], 
        end = input[["colour_scale_range"]][2]
      )
    } else {
      ggplot2::scale_fill_viridis_d(
        option = tolower(input[["colour_scale"]]), 
        begin = input[["colour_scale_range"]][1], 
        end = input[["colour_scale_range"]][2]
      )
    }
  })

  ## Upload tab ------------------------------------------------------------------------------------
  output$template <- shiny::downloadHandler(
    filename = function() "endoc-bh1_template.xlsx",
    content = function(file) file.copy(file.path("www", "template.xlsx"), file, overwrite = TRUE)
  )
  
  xlsx_contents_summary <- shiny::reactive({
    if (!is.null(input[["xlsx_files"]]) && nrow(input[["xlsx_files"]]) > 0) {
      res_copy <- purrr::pmap(input[["xlsx_files"]], function(name, size, type, datapath) {
        file.copy(datapath, to = file.path("www", "xlsx", name), overwrite = TRUE)
      })
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
    }
  })
  
  output$xlsx_contents_summary <- shiny::renderUI({ shiny::req(xlsx_contents_summary()) })

  xlsx_available <- shiny::reactive({
    uploaded_files <- xlsx_contents_summary()
    dplyr::tibble(file = list.files(path = file.path("www", "xlsx"), pattern = ".xlsx$", full.names = TRUE)) %>%
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
  
  xlsx_contents <- shiny::reactive({
    id <- shiny::showNotification(
      ui = "Data are loading, please wait ...", 
      duration = NULL, closeButton = FALSE
    )
    on.exit(shiny::removeNotification(id), add = TRUE)
    if (length(xlsx_available()[["file"]]) != 0) {
      get_xlsx_contents(
        files = xlsx_available()[["file"]],
        od_outlier = input[["od_outlier"]] %||% 3, 
        lm_outlier = input[["lm_outlier"]] %||% 1.5,
        project_name = input[["project_name"]] %||% NULL
      )
    }
  })
  
  output$project_ui <- shiny::renderUI({
    shiny::selectInput("project_name", "Available Projects",
      choices = unique(xlsx_available()[["Project"]]),
      selected = shiny::isolate(input[["project_name"]]),
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


  ## Blank tab -------------------------------------------------------------------------------------
  
  od_box_plot <- shiny::reactive({
    shiny::req(xlsx_contents())
    ggplot2::ggplot(
      data = dplyr::bind_rows(xlsx_contents(), dplyr::mutate(xlsx_contents(), Step = "ALL")),
      mapping = ggplot2::aes(x = .data[["Step"]], y = .data[["re_OD"]], colour = .data[["Step"]])
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::labs(
        x = NULL,
        y = bquote("Relative Error:"~frac((OD[2] - OD[1]), OD[1]))
      ) +
      ggplot2::theme(legend.position = "none")
  })

  od_density_plot <- shiny::reactive({
    shiny::req(xlsx_contents())
    ggplot2::ggplot(
      data = xlsx_contents(),
      mapping = ggplot2::aes(x = .data[["re_OD"]], colour = .data[["Step"]], fill = .data[["Step"]])
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::labs(
        x = bquote("Relative Error:"~frac((OD[2] - OD[1]), OD[1])),
        y = "Density",
        colour = NULL,
        fill = NULL
      )
  })
  
  od_lm_box_plot <- shiny::reactive({
    shiny::req(xlsx_contents())
    ggplot2::ggplot(
      data = xlsx_contents() %>% 
        dplyr::select("filename", dplyr::matches("_Intercept|_Slope")) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(
          cols = -c("filename"), 
          names_to = c(".value", "term"),
          names_pattern = "(.*)_([^_]*)",
        ),
      mapping = ggplot2::aes(x = .data[["term"]], y = .data[["estimate"]], group = .data[["term"]])
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
      ggplot2::labs(x = NULL, y = "Estimate") +
      ggplot2::facet_wrap(facets = ggplot2::vars(.data[["term"]]), scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
  })
  
  od_lm_line_plot <- shiny::reactive({
    shiny::req(xlsx_contents())
    ggplot2::ggplot(
      data = xlsx_contents() %>% 
        dplyr::filter(.data[["Step"]] == "BLANK" & .data[["Concentration (µg/L)"]] != 0),
      mapping = ggplot2::aes(
        x = .data[["Concentration (µg/L)"]], 
        y = .data[["normalised_OD"]], 
        colour = .data[["filename"]]
      )
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
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
    xlsx_contents_subset <- dplyr::filter(shiny::req(xlsx_contents()), Step != "BLANK")
    
    shiny::selectInput("targets_list", "Available Files",
      choices = unique(xlsx_contents_subset[["filename"]]),
      selected = unique(xlsx_contents_subset[["filename"]]),
      width = "100%",
      selectize = FALSE,
      multiple = TRUE,
      size = 10
    )
  })
  
  output$experiment_ui <- shiny::renderUI({ 
    experiments_list <- shiny::req(xlsx_contents()) %>% 
      dplyr::mutate(experiment_file = glue::glue("{Target} ({filename})")) %>% 
      dplyr::filter(
        .data[["filename"]] %in% shiny::req(input[["targets_list"]]), 
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
  
  xlsx_contents_selected <- shiny::reactive({
    shiny::req(xlsx_contents())
    
    experiments_list <- xlsx_contents() %>% 
      dplyr::filter(
        .data[["filename"]] %in% input[["targets_list"]], 
        !.data[["Step"]] %in% "BLANK"
      )
    
    dplyr::full_join(
      x = experiments_list,
      y = experiments_list %>% 
        dplyr::filter(Type %in% "Reference") %>% 
        dplyr::group_by(.data[["filename"]]) %>% 
        dplyr::summarise(
          is_reference_good = mean(.data[["fc_sn2_sn1"]], na.rm = TRUE) >= input[["fold_change"]]
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(c("filename", "is_reference_good")),
      by = "filename"
    ) %>% 
      dplyr::mutate(
        experiment_file = glue::glue("{Target} ({filename})"),
        keep = .data[["experiment_file"]] %in% !!input[["experiments_list"]]
      )
  })

  is_ratio_distribution_plot <- shiny::reactive({
    shiny::req(xlsx_contents_selected())
    shiny::req(length(intersect(input[["experiments_list"]], xlsx_contents_selected()[["experiment_file"]])) != 0)
    xlsx_contents_selected <- dplyr::filter(xlsx_contents_selected(), !.data[["is_any_outlier"]])

    if (nrow(xlsx_contents_selected) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2_theme() +
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

    fc_threshold <- input[["fold_change"]]

    gg_data <- xlsx_contents_selected %>%
      dplyr::filter(
        .data[["Type"]] %in% "Reference",
        !.data[["is_any_outlier"]],
        .data[["Step"]] %in% "SN2"
      ) %>%
      dplyr::select(c("filename", "Type_Target", "Condition", "fc_sn2_sn1")) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(facet_file = gsub(".xlsx$", "", gsub("_", "\n", .data[["filename"]])))

    ggplot2::ggplot(
      data = gg_data,
      mapping = ggplot2::aes(
        x = .data[["Type_Target"]],
        y = .data[["fc_sn2_sn1"]]
      )
    ) +
      ggplot2_theme() +
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
      ggplot2::geom_boxplot(alpha = 0.2, width = 0.5, outlier.shape = NA) +
      ggplot2::geom_violin(alpha = 0.2) +
      ggbeeswarm::geom_quasirandom(
        mapping = ggplot2::aes(
          colour = .data[["Condition"]],
          fill = .data[["Condition"]],
          shape = .data[["Condition"]]
        ),
        size = input[["point_size"]]
      ) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::scale_shape() +
      ggplot2::labs(
        x = NULL,
        y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")"))),
        colour = "Condition",
        fill = "Condition",
        shape = "Condition",
        caption = if (all(xlsx_contents_selected[["is_reference_good"]])) {
          NULL
        } else {
          "Warning: Reference is not secreting insulin in one or several experiments!"
        }
      ) +
      # ggplot2::theme(legend.position = "none") +
      ggplot2::theme(plot.caption = ggplot2::element_text(colour = "firebrick2"))
  })

  is_od_plot <- shiny::reactive({
    shiny::req(xlsx_contents_selected())
    shiny::req(length(intersect(input[["experiments_list"]], xlsx_contents_selected()[["experiment_file"]])) != 0)
    xlsx_contents_selected <- dplyr::filter(xlsx_contents_selected(), .data[["keep"]] & !.data[["is_any_outlier"]])

    if (nrow(xlsx_contents_selected) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2_theme() +
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
      data = xlsx_contents_selected %>%
        dplyr::select(c("Sample", "Step", "Type_Target", "normalised_OD", "Date", "Operator")) %>%
        dplyr::distinct(),
      mapping = ggplot2::aes(
        x = paste0(.data[["Step"]], "\n", .data[["Sample"]]),
        y = .data[["normalised_OD"]],
        colour = .data[["Type_Target"]],
        fill = .data[["Type_Target"]],
        group = .data[["Type_Target"]]
      )
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
      ggplot2::labs(
        x = NULL,
        y = "Normalised Optical Density\n(OD)",
        colour = NULL,
        fill = NULL,
        caption = if (all(xlsx_contents_selected[["is_reference_good"]])) {
          NULL
        } else {
          "Warning: Reference is not secreting insulin in one or several experiments!"
        }
      ) +
      ggplot2::theme(plot.caption = ggplot2::element_text(colour = "firebrick2"))
  })

  is_percent_plot <- shiny::reactive({
    shiny::req(xlsx_contents_selected())
    shiny::req(length(intersect(input[["experiments_list"]], xlsx_contents_selected()[["experiment_file"]])) != 0)
    xlsx_contents_selected <- dplyr::filter(xlsx_contents_selected(), .data[["keep"]] & !.data[["is_any_outlier"]])

    if (nrow(xlsx_contents_selected) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2_theme() +
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
      data = xlsx_contents_selected %>%
        dplyr::filter(Step %in% c("SN1", "SN2")) %>% 
        dplyr::select(c("Sample", "Type_Target", "Insulin Secretion (% of content)", "Date", "Operator")) %>%
        dplyr::distinct(),
      mapping = ggplot2::aes(
        x = .data[["Sample"]],
        y = .data[["Insulin Secretion (% of content)"]],
        colour = .data[["Type_Target"]],
        fill = .data[["Type_Target"]]
      )
    ) +
      ggplot2_theme() +
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
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        limits = c(0, NA),
        expand = ggplot2::expand_scale(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Insulin Secretion\n(% of content)",
        colour = NULL,
        fill = NULL,
        caption = if (all(xlsx_contents_selected[["is_reference_good"]])) {
          NULL
        } else {
          "Warning: Reference is not secreting insulin in one or several experiments!"
        }
      ) +
      ggplot2::theme(plot.caption = ggplot2::element_text(colour = "firebrick2"))
  })

  is_ratio_plot <- shiny::reactive({
    shiny::req(xlsx_contents_selected())
    shiny::req(length(intersect(input[["experiments_list"]], xlsx_contents_selected()[["experiment_file"]])) != 0)
    xlsx_contents_selected <- dplyr::filter(xlsx_contents_selected(), .data[["keep"]] & !.data[["is_any_outlier"]])

    if (nrow(xlsx_contents_selected) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2_theme() +
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

    gg_data <- xlsx_contents_selected %>%
      tidyr::drop_na(.data[["Type"]], .data[["Target"]]) %>%
      dplyr::select(c("Condition", "Type_Target", "fc_sn2_sn1", "Date", "Operator")) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(.data[["fc_sn2_sn1"]])

    if (length(unique(gg_data[["Type_Target"]])) > 1) {
      lm_data <- gg_data %>%
        dplyr::group_by(.data[["Condition"]]) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          lm = purrr::map(.x = .data[["data"]], .f = function(data) {
            if (length(unique(data[["Type_Target"]])) > 1) {
              purrr::map2_df(
                .x = utils::combn(levels(droplevels(data[["Type_Target"]])), 2, simplify = FALSE),
                .y = list(data),
                .f = function(x, data) {
                  mult <- 1.5
                  splitted_data <- dplyr::filter(data, .data[["Type_Target"]] %in% x)
                  mean_se_fc <- splitted_data %>%
                    dplyr::group_by(.data[["Type_Target"]]) %>%
                    dplyr::summarise(
                      mean_se = mean(.data[["fc_sn2_sn1"]]) +
                        sqrt(stats::var(.data[["fc_sn2_sn1"]]) / length(.data[["fc_sn2_sn1"]]))
                    ) %>%
                    dplyr::ungroup()
  
                  default_formula <- fc_sn2_sn1 ~ Type_Target
                  if (length(unique(splitted_data[["Date"]])) > 1) {
                    splitted_data[["Date"]] <- as.factor(splitted_data[["Date"]])
                    default_formula <- stats::update.formula(default_formula, . ~ . + Date)
                  }
                  if (length(unique(splitted_data[["Operator"]])) > 1) {
                    splitted_data[["Operator"]] <- as.factor(splitted_data[["Operator"]])
                    default_formula <- stats::update.formula(default_formula, . ~ . + Operator)
                  }
                  stats::lm(default_formula, data = splitted_data) %>%
                    broom::tidy() %>%
                    dplyr::filter(grepl("Type_Target", .data[["term"]])) %>%
                    dplyr::mutate(
                      group1 = levels(droplevels(splitted_data[["Type_Target"]]))[1],
                      group2 = levels(droplevels(splitted_data[["Type_Target"]]))[2],
                      y_position = max(mean_se_fc[["mean_se"]]),
                      term = NULL
                    )
                }
              ) %>%
                dplyr::mutate_at(
                  .vars = dplyr::vars(dplyr::num_range("group", 1:2)),
                  .funs = ~ factor(.x, levels = levels(data[["Type_Target"]]))
                )
            }
          }),
          data = NULL
        ) %>%
        tidyr::unnest("lm") %>%
        dplyr::group_by(.data[["Condition"]]) %>%
        dplyr::mutate(
          group = 1:dplyr::n(),
          y_position = max(.data[["y_position"]]),
          y_position = .data[["y_position"]] + c(0.5 * .data[["group"]]),
          annotations = paste("p =", format.pval(.data[["p.value"]], digits = 3))
        ) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na(.data[["p.value"]])
    }

    ggplot2::ggplot(
      data = gg_data,
      mapping = ggplot2::aes(
        x = .data[["Type_Target"]],
        y = .data[["fc_sn2_sn1"]],
        colour = .data[["Type_Target"]],
        fill = .data[["Type_Target"]],
        group = .data[["Type_Target"]]
      )
    ) +
      ggplot2_theme() +
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
          se <- sqrt(stats::var(x)/length(x))
          mean <- mean(x)
          data.frame(y = mean, label = length(x))
        },
        position = ggplot2::position_dodge(width = 0.9),
        fill = "white",
        colour = "black",
        show.legend = FALSE,
        vjust = 1.25
      ) +
      ggplot2_colour() +
      ggplot2_fill() +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expand_scale(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        x = NULL,
        y = bquote(atop("Insulin Secretion", group("(", "SN2"/"SN1", ")"))),
        colour = NULL,
        fill = NULL,
        caption = if (all(xlsx_contents_selected[["is_reference_good"]])) {
          NULL
        } else {
          "Warning: Reference is not secreting insulin in one or several experiments!"
        }
      ) +
      {
        if (length(unique(gg_data[["Type_Target"]])) > 1 && nrow(lm_data) > 0) {
          ggsignif::geom_signif(
            data = dplyr::mutate(lm_data, group = 1:dplyr::n()),
            mapping = ggplot2::aes(
              y_position = .data[["y_position"]],
              xmin = .data[["group1"]], xmax = .data[["group2"]],
              annotations = .data[["annotations"]],
              group = .data[["group"]]
            ),
            manual = TRUE,
            tip_length = 0,
            inherit.aes = FALSE
          )
        }
      } +
      ggplot2::facet_grid(cols = ggplot2::vars(!!ggplot2::sym("Condition"))) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(plot.caption = ggplot2::element_text(colour = "firebrick2"))
  })

  is_plot <- shiny::reactive({
    ggpubr::ggarrange(
      is_od_plot(), is_percent_plot(),
      nrow = 1, ncol = 2, align = "v",
      legend = "top", common.legend = TRUE
    )
  })
  
  shiny::observeEvent(input[["show_issues"]], {
    shiny::showModal(shiny::modalDialog(size = "l", easyClose = TRUE,
      title = "Issues Detected in the Selected Experiments",
      {
        list_issues <- get_outliers(dplyr::filter(xlsx_contents_selected(), .data[["keep"]]), input[["fold_change"]])
        if (nrow(list_issues)==0) {
          shiny::tags$p("No issues currently detected in the selected experiments.")
        } else {
          DT::renderDataTable({ list_issues })
        }
      }
    ))
  })
  
  
  ## Outliers tab ----------------------------------------------------------------------------------
  outliers_list <- shiny::reactive({
    get_outliers(shiny::req(xlsx_contents()), input[["fold_change"]])
  })
  
  output$outliers <- DT::renderDataTable({ outliers_list() })
}

shiny::shinyApp(ui, server)
