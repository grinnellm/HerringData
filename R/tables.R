#' Default table.
#'
#' Default data summary report table.
#'
#' @param dat Tibble. Data for the table.
#' @param cap Character. Table caption. Default is "Table caption."
#' @importFrom Rdpack reprompt
#' @return kable of dat for RMarkdown.
#' @family tables
#' @export
report_table <- function(dat, cap = "Table caption."){
  # kdat <- kable(x = dat, caption = cap, booktabs = TRUE) %>%
  #   column_spec(0, bold = T)
  # kdat
}
