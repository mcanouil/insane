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
  out_all_excel <- data.table::rbindlist(lapply(
    X = files,
    FUN = function(.file) {
      data.table::rbindlist(lapply(
        X = readxl::excel_sheets(.file),
        .file = .file,
        FUN = function(.sheet, .file) {
          tmp <- suppressMessages(readxl::read_xlsx(.file, .sheet, progress = FALSE))
          tmp <- data.table::setDT(tmp)[
            j = .SD, 
            .SDcols = 1:max(grep("Volume \\(µl\\)|OD2", colnames(tmp)))
          ]
          tmp[
            j = `:=`(
              filename = basename(.file),
              sheet_name = .sheet,
              re_OD = (OD2 - OD1) / OD1,
              mean_OD =  mean(c(OD1, OD2))
            ),
            by = 1:nrow(tmp)
          ]
          
          tmp
        }
      ), fill = TRUE)
    }
  ))

  if (is.null(project_name)) project_name <- unique(out_all_excel[["Project"]])
  
  out_excel <- out_all_excel[
    i = Project %in% project_name
  ][
    j = `:=`("normalised_OD" = mean_OD - mean_OD[1]),
    by = "filename"
  ][
    j = `:=`(
      is_outlier_OD = (function(x) {
        xq <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        xiqr <- stats::IQR(x, na.rm = TRUE)
        x < xq[1] - od_outlier * xiqr | x > xq[2] + od_outlier * xiqr
      })(re_OD) | is.na(normalised_OD)
    )
  ][
    i = Step %in% "BLANK" & `Concentration (mU/L)` != 0 & (!is_outlier_OD),
    j = c("Intercept", "Slope") := data.table::transpose(list(
      broom::tidy(
        stats::lm(
          formula = log10(normalised_OD) ~ log10(`Concentration (mU/L)` / 23),
          data = .SD
        )
      )[["estimate"]]
    )),
    by = "filename"
  ][
    i = Step %in% "BLANK" & `Concentration (mU/L)` != 0 & (!is_outlier_OD),
    j = paste0("is_outlier_", c("Intercept", "Slope")) := lapply(
      X = .SD, 
      FUN = function(x) {
        xq <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        xiqr <- stats::IQR(x, na.rm = TRUE)
        x < xq[1] - od_outlier * xiqr | x > xq[2] + od_outlier * xiqr
      }
    ),
    .SDcols = c("Intercept", "Slope")
  ][
    j = c("Intercept", "Slope") := lapply(.SD, function(x) unique(x[!is.na(x)])),
    .SDcols = c("Intercept", "Slope"),
    by = "filename"
  ][
    j = `:=`(
      "Concentration (µg/L)" = {
        ctmp <- `Dilution Factor` * 10^((log10(normalised_OD) - Intercept) / Slope)
        data.table::fifelse(
          test = is.na(ctmp) & Step == "BLANK",
          yes = `Concentration (mU/L)` / 23,
          no = ctmp
        )
      }
    )
  ][
    j = c("is_outlier_OD", "is_outlier_Intercept", "is_outlier_Slope") := 
      lapply(X = .SD, FUN = function(x) sapply(x, isTRUE)),
    .SDcols = c("is_outlier_OD", "is_outlier_Intercept", "is_outlier_Slope")
  ][
    j = `:=`(
      "measure_id" = 1:.N,
      "Total (ng)" = `Volume (µl)` / 10^3 * `Concentration (µg/L)`,
      is_any_outlier = is_outlier_OD | is_outlier_Intercept | is_outlier_Slope
    ), 
    by = c("filename", "sheet_name", "Target", "Step")
  ]
  
  out_insulin <- data.table::dcast(
    data = out_excel[(!is_any_outlier)], 
    formula = filename + sheet_name + Target + measure_id ~ Step, 
    value.var = "Total (ng)", 
  )[
    j = list(
      filename, sheet_name,  Target, measure_id,
      ins_SUPERNATANT1 = SUPERNATANT1 / (LYSATE + SUPERNATANT1 + SUPERNATANT2),
      ins_SUPERNATANT2 = SUPERNATANT2 / (LYSATE + SUPERNATANT2)
    )
  ][
    j = `:=`("fc_SUPERNATANT2_SUPERNATANT1" = ins_SUPERNATANT2 / ins_SUPERNATANT1),
    by = c("filename", "sheet_name", "Target", "measure_id")
  ]
  
  out_excel_insulin <- merge(
    x = out_excel, 
    y = data.table::melt(
      data = out_insulin, 
      id.vars = c("filename", "sheet_name", "Target", "measure_id", "fc_SUPERNATANT2_SUPERNATANT1"),
      measure.vars = patterns("ins_SUPERNATANT[1-2]"),
      variable.name = "Step", 
      value.name =  "Insulin Secretion (% of content)"
    )[
      i = !is.na(`Insulin Secretion (% of content)`)
    ][
      j = `:=`(
        "Step" = gsub("ins_", "", Step),
        "log2_fc_SUPERNATANT2_SUPERNATANT1" = log2(fc_SUPERNATANT2_SUPERNATANT1)
      )
    ], 
    by = c("filename", "sheet_name", "Step", "Target", "measure_id"), 
    all = TRUE
  )[
    j = `:=`(
      "is_any_outlier" = is_outlier_OD | is_outlier_Intercept | is_outlier_Slope | 
        (is.na(fc_SUPERNATANT2_SUPERNATANT1) & !Step %in% c("BLANK", "LYSATE")) |
        (is.na(`Insulin Secretion (% of content)`) & !Step %in% c("BLANK", "LYSATE")),
      "Sample" = gsub("mM ", "mM\n", Sample)
    )
  ]
  
  data.table::setorderv(
    x = out_excel_insulin, 
    cols = c("filename", "sheet_name", "Condition", "Sample")
  )[
    j = `:=`(
      "Sample" = factor(Sample, levels = unique(Sample)),
      "Type" = factor(
        x = (function(x) { substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x })(Type), 
        levels = c("Reference", "Control", "Target")
      )
    )
  ][
    j = `:=`(
      "Type_Target" = gsub(
        pattern = "NA: NA", 
        replacement = NA_character_, 
        x = paste0(Type, ": ", Target)
      )
    )
  ]
  
  data.table::setorderv(x = out_excel_insulin, cols = c("Type", "Target"))[
    j = `:=`("Type_Target" = factor(x = Type_Target, levels = unique(Type_Target)))
  ]
  
  out_excel_insulin
}
