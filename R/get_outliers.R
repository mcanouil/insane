#' Extract Outliers
#'
#' @param data A data.frame.
#' @param fold_change A numeric.
#'
#' @return A data.frame.
#' @export
get_outliers <- function(data, fold_change) {
  `%>%` <- dplyr::`%>%`
  if (!"is_reference_good" %in% colnames(data)) {
    data <- dplyr::full_join(
      x = data,
      y = data %>% 
        dplyr::filter(Type %in% "Reference") %>% 
        dplyr::group_by(.data[["filename"]]) %>% 
        dplyr::summarise(
          is_reference_good = mean(.data[["fc_SUPERNATANT2_SUPERNATANT1"]], na.rm = TRUE) >= fold_change,
          .groups = "drop_last"
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
      "Not Secreting Insulin in Reference" = any(!.data[["is_reference_good"]]),
      .groups = "drop_last"
    )
}
