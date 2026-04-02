#' @param fill_missing Logical. Fill missing weight-at-age data: missing values
#'   are imputed as the mean of the previous `n_roll` years; missing values at
#'   the beginning of the time series are imputed by extending the first
#'   non-missing value backwards. Default TRUE.
