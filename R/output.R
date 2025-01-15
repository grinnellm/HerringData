#' Input file for SISCAH: catch data.
#'
#' Make the input file for SISCAH: catch data.
#'
#' @template param-catch
#' @template param-structure
#' @template param-file
#' @family SISCAH
#' @export
siscah_catch <- function(catch, structure = "Region", file) {
  # Wrangle data
  res <- catch %>%
    group_by(Year, Period, StatArea) %>%
    summarise(Catch = sum(Catch) / 1000) %>%
    ungroup() %>%
    rename(Gear = Period, Value = Catch, Stock = StatArea) %>%
    select(Year, Gear, Value, Stock) %>%
    arrange(Stock, Gear, Year)
  # Return catch
  return(res)
} # End siscah_catch function
