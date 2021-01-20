files = list.files(path = "inst/extdata/", pattern = ".xlsx$", full.names = TRUE)
project_name = NULL
od_outlier = 1.5
lm_outlier = 1.5

# get_xlsx_contents <- function(files, project_name = NULL, od_outlierout_all_excel = 1.5, lm_outlier = 1.5) {
  
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
  
  out_excel <- out_all_excel[Project %in% project_name][
    j = `:=`(
      is_outlier_OD = (function(x) {
        xq <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        xiqr <- stats::IQR(x, na.rm = TRUE)
        x < xq[1] - od_outlier * xiqr | x > xq[2] + od_outlier * xiqr
      })(re_OD)
    )
  ][
    j = `:=`(
      normalised_OD = mean_OD - mean_OD[1]
    ),
    by = "filename"
  ][
    Step %in% "BLANK" & `Concentration (mU/L)` != 0 & (!is_outlier_OD)
  ][
    j = c("Intercept", "Slope") := transpose(list(
      broom::tidy(
        stats::lm(
          formula = log10(normalised_OD) ~ log10(`Concentration (mU/L)` / 23),
          data = .SD
        )
      )[["estimate"]]
    )),
    by = "filename"
  ][
    j = paste0("is_outlier_", c("Intercept", "Slope")) := lapply(
      X = .SD, 
      FUN = function(x) {
        xq <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        xiqr <- stats::IQR(x, na.rm = TRUE)
        x < xq[1] - od_outlier * xiqr | x > xq[2] + od_outlier * xiqr
      }
    ),
    .SDcols = c("Intercept", "Slope")
  ]
  
  
# }
