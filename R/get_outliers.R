#' Extract Outliers
#'
#' @param data A data.frame. 
#'     A `data.frame` from `get_xlsx_contents` in which 
#'     to extract measurements identified as outliers.
#' @param fold_change A numeric. 
#'     Fold-change threshold for reference above 
#'     which the reference is considered 'good'.
#'
#' @return A data.frame.
#' @export
get_outliers <- function(data, fold_change) {
  # ---------------------
  # Fix no visible binding for global variable from data.table
  Type <- fc_SUPERNATANT2_SUPERNATANT1 <- is_reference_good <- NULL
  is_any_outlier <- is_outlier_Intercept <- is_outlier_Slope <- NULL
  is_outlier_OD <- NULL
  # ---------------------
  
  if (!inherits(x = data, what = "data.table")) data <- data.table::as.data.table(data)
  
  if (!"is_reference_good" %in% colnames(data)) {
    data[
      i = data[
        i = Type %in% "Reference",
        j = list(
          "is_reference_good" = mean(fc_SUPERNATANT2_SUPERNATANT1, na.rm = TRUE) >= fold_change
        ), 
        by = "filename"
      ],
      j = is_reference_good := is_reference_good,
      on = "filename"
    ]
  }
  
  data[
    i = (is_any_outlier) | (!is_reference_good),
    j = c(
      "filename", "Target", "Condition", 
      "is_outlier_Intercept", "is_outlier_Slope", 
      "is_outlier_OD",
      "is_reference_good"
    )
  ][
    j = list(
      "Blank Linearity" = any(is_outlier_Intercept | is_outlier_Slope),
      "Technical Variability (OD)" = any(is_outlier_OD),
      "Not Secreting Insulin in Reference" = any(!is_reference_good)
    ),
    by = "filename"
  ]
  
  data.table::setDF(data)
}
