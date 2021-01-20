#' Read And Tidy Experiments Spreadsheets
#'
#' @param files A character.
#' @param project_name A character.
#' @param od_outlier A numeric.
#' @param lm_outlier A numeric.
#'
#' @return A data.frame.
#' @export
get_xlsx_contents <- function(files, project_name = NULL, od_outlier = 1.5, lm_outlier = 1.5) {
  `%>%` <- dplyr::`%>%`
  
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
    tidyr::unnest(cols = .data[["nested_data"]])
  
  if (is.null(project_name)) project_name <- unique(out_all_excel[["Project"]])
  
  out_excel <- out_all_excel %>%
    dplyr::filter(.data[["Project"]] %in% !!project_name) %>% 
    dplyr::mutate(
      lower_threshold = stats::quantile(.data[["re_OD"]], 0.25, na.rm = TRUE) - 
        !!od_outlier * stats::IQR(.data[["re_OD"]], na.rm = TRUE),
      upper_threshold = stats::quantile(.data[["re_OD"]], 0.75, na.rm = TRUE) + 
        !!od_outlier * stats::IQR(.data[["re_OD"]], na.rm = TRUE),
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
      .dta <- .
      lm = broom::tidy(
        stats::lm(
          formula = log10(normalised_OD) ~ log10(`Concentration (mU/L)` / 23),
          data = .dta
        )
      )[, c("term", "estimate")]
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      term = c("(Intercept)" = "Intercept", "log10(`Concentration (mU/L)`/23)" = "Slope")[.data[["term"]]]
    ) %>% 
    dplyr::group_by(.data[["term"]]) %>% 
    dplyr::mutate(
      lower_threshold = stats::quantile(.data[["estimate"]], 0.25) - !!lm_outlier * stats::IQR(.data[["estimate"]]),
      upper_threshold = stats::quantile(.data[["estimate"]], 0.75) + !!lm_outlier * stats::IQR(.data[["estimate"]]),
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
      ins_SUPERNATANT1 = .data[["SUPERNATANT1"]] / (.data[["LYSATE"]] + .data[["SUPERNATANT1"]] + .data[["SUPERNATANT2"]]),
      ins_SUPERNATANT2 = .data[["SUPERNATANT2"]] / (.data[["LYSATE"]] + .data[["SUPERNATANT2"]])
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select("filename", "sheet_name", "Target", "measure_id", dplyr::num_range("ins_SUPERNATANT", 1:2))
  
  out_insulin <- out_insulin_tmp %>% 
    tidyr::pivot_longer(
     cols = dplyr::num_range("ins_SUPERNATANT", 1:2), 
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
    tidyr::drop_na(dplyr::num_range("ins_SUPERNATANT", 1:2)) %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]], .data[["Target"]], .data[["measure_id"]]) %>% 
    dplyr::transmute(
      "fc_SUPERNATANT2_SUPERNATANT1" = .data[["ins_SUPERNATANT2"]] / .data[["ins_SUPERNATANT1"]],
      "log2_fc_SUPERNATANT2_SUPERNATANT1" = log2(.data[["ins_SUPERNATANT2"]] / .data[["ins_SUPERNATANT1"]])
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
        (is.na(.data[["fc_SUPERNATANT2_SUPERNATANT1"]]) & !.data[["Step"]] %in% c("BLANK", "LYSATE")) |
        (is.na(.data[["Insulin Secretion (% of content)"]]) & !.data[["Step"]] %in% c("BLANK", "LYSATEE")),
      Sample = gsub("mM ", "mM\n", .data[["Sample"]])
    ) %>% 
    dplyr::group_by(.data[["filename"]], .data[["sheet_name"]]) %>% 
    dplyr::arrange(.data[["Condition"]], .data[["Sample"]]) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      Sample = factor(.data[["Sample"]], levels = unique(.data[["Sample"]])),
      Type = factor(
        x = (function(x) { substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x })(.data[["Type"]]), 
        levels = c("Reference", "Control", "Target")
      ),
    ) %>% 
    dplyr::arrange(.data[["Type"]], .data[["Target"]]) %>% 
    dplyr::mutate(
      Type_Target = gsub("NA: NA", NA, paste0(.data[["Type"]], ": ", .data[["Target"]])),
      Type_Target = factor(x = .data[["Type_Target"]], levels = unique(.data[["Type_Target"]]))
    )
}
