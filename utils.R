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

get_xlsx_contents <- function(path, od_outlier, lm_outlier) {
  out_excel <- dplyr::tibble(file = list.files(path, full.names = TRUE)) %>%
    dplyr::mutate(sheet_name = purrr::map(.data[["file"]], readxl::excel_sheets)) %>%
    tidyr::unnest(.data[["sheet_name"]]) %>%
    dplyr::mutate(
      nested_data = purrr::map2(.data[["file"]], .data[["sheet_name"]], function(.x, .y) {
        tmp <- suppressMessages({readxl::read_xlsx(.x, .y)})
        dplyr::mutate(tmp[, 1:grep("Operator", colnames(tmp))],
          is_blank = NULL,
          re_OD = (.data[["OD2"]] - .data[["OD1"]]) / .data[["OD1"]],
          mean_OD = purrr::map2_dbl(.data[["OD1"]], .data[["OD2"]], ~ mean(c(.x, .y)))
        )
      })
    ) %>%
    tidyr::unnest(nested_data) %>%
    dplyr::mutate(
      is_blank = .data[["Step"]] == "BLANK",
      lower_threshold = stats::quantile(.data[["re_OD"]], 0.25) - od_outlier * stats::IQR(.data[["re_OD"]]),
      upper_threshold = stats::quantile(.data[["re_OD"]], 0.75) + od_outlier * stats::IQR(.data[["re_OD"]]),
      is_outlier = .data[["re_OD"]] < .data[["lower_threshold"]] | .data[["re_OD"]] > .data[["upper_threshold"]]
    ) %>%
    dplyr::group_by(.data[["file"]]) %>%
    dplyr::mutate(
      wrong_blank = any(.data[["is_outlier"]] & .data[["Concentration (mU/L)"]] %in% 0),
      is_outlier = ifelse(.data[["wrong_blank"]], TRUE, .data[["is_outlier"]]),
      normalised_OD = .data[["mean_OD"]] - .data[["mean_OD"]][1]
    ) %>%
    dplyr::ungroup()
  
  out_model <- out_excel %>% 
    dplyr::filter(
      .data[["Step"]] == "BLANK",
      .data[["Concentration (mU/L)"]] != 0,
      !.data[["is_outlier"]]
    ) %>% 
    dplyr::group_by(.data[["file"]]) %>% 
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
      id_cols = "file", 
      names_from = "term", 
      values_from = c("estimate", "lower_threshold", "upper_threshold", "is_outlier")
    )
  
  out_excel_model <- dplyr::full_join(x = out_excel, y = out_model, by = "file") %>% 
    dplyr::group_by(.data[["file"]]) %>% 
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
    dplyr::group_by(.data[["file"]], .data[["sheet_name"]], .data[["Target"]], .data[["Step"]]) %>% 
    dplyr::mutate(measure_id = 1:dplyr::n()) %>% 
    dplyr::ungroup()
  
  out_insulin_tmp <- out_excel_model %>% 
    dplyr::group_by(.data[["file"]], .data[["sheet_name"]], .data[["Target"]]) %>% 
    tidyr::pivot_wider(
      id_cols = c("file", "sheet_name", "Target", "measure_id"), 
      names_from = "Step", 
      values_from = "Total (ng)"
    ) %>% 
    dplyr::mutate(
      ins_SN1 = .data[["SN1"]] / (.data[["LYSAT"]] + .data[["SN1"]] + .data[["SN2"]]),
      ins_SN2 = .data[["SN2"]] / (.data[["LYSAT"]] + .data[["SN2"]])
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select("file", "sheet_name", "Target", "measure_id", tidyr::num_range("ins_SN", 1:2))
  
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
    by = c("file", "sheet_name", "Step", "Target", "measure_id")
  )
  
  out_insulin_fc <- out_insulin_tmp %>% 
    tidyr::drop_na(tidyr::num_range("ins_SN", 1:2)) %>% 
    dplyr::group_by(.data[["file"]], .data[["sheet_name"]], .data[["Target"]], .data[["measure_id"]]) %>% 
    dplyr::transmute("fc_sn2_sn1" = log2(ins_SN2 / ins_SN1)) %>% 
    dplyr::ungroup()
  
  dplyr::full_join(
    x = out_excel_insulin, 
    y = out_insulin_fc, 
    by = c("file", "sheet_name", "Target", "measure_id")
  ) %>% 
    dplyr::mutate(
      is_any_outlier = .data[["is_outlier"]] | .data[["is_outlier_Intercept"]] | .data[["is_outlier_Slope"]],
      Sample = gsub("mM ", "mM\n", .data[["Sample"]])
    )
}
