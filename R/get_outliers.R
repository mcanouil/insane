#' Extract Outliers
#'
#' @param data A data.frame.
#' @param fold_change A numeric.
#'
#' @return A data.frame.
#' @export
get_outliers <- function(data, fold_change) {
  if (!inherits(x = data, what = "data.table")) data <- as.data.table(data)
  
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
  
  setDF(data)
}
