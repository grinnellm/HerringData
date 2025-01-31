#' SISCAH input: biological data.
#'
#' Make the input files for SISCAH: biological data (proportion- and
#' weight-at-age).
#'
#' @template param-bio
#' @template param-structure
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @return List of tibbles with biological data aggregated by Year, Period, and
#'   `structure`, including spatial and temporal information. List has two
#'   tibbles: proportion-at-age and weight-at-age in grams.
#' @family SISCAH
#' @export
siscah_bio <- function(
    catch, structure = `Region`, kilo = TRUE, n_digits = 3
) {
  # TODO: Check for backticks around structure, otherwise error
  # Determine stocks
  stocks <- catch %>% pull({{structure}}) %>% unique
  # Determine prefix
  stock_prefix <- catch %>% select({{structure}}) %>% names()
  # Get number of groups in structure and names
  n_stocks <- tibble(
    Stock = stocks,
    Area = 1:length(stocks),
    Name = paste(stock_prefix, stocks, sep = "")
  )
  # Wrangle data
  res <- catch %>%
    group_by(Year, Period, {{structure}}) %>%
    summarise(Catch = sum(Catch) / ifelse(kilo, 1000, 1)) %>%
    ungroup() %>%
    # TODO: Some of this is just filler
    mutate(Type = 1, Catch = round(Catch, digits = n_digits)) %>%
    rename(Gear = Period, Value = Catch, Stock = {{structure}}) %>%
    full_join(y = n_stocks, by = "Stock") %>%
    select(Year, Gear, Area, Type, Value, Name) %>%
    rename(Stock = Name) %>%
    arrange(Stock, Gear, Year)
  # Return catch
  return(res)
} # End siscah_bio function

#' SISCAH input: catch data.
#'
#' Make the input file for SISCAH: catch data.
#'
#' @template param-catch
#' @template param-structure
#' @template param-kilo
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @return Tibble with catch data aggregated by Year, Period, and `structure`,
#'   including spatial and temporal information.
#' @family SISCAH
#' @export
siscah_catch <- function(
    catch, structure = `Region`, kilo = TRUE, n_digits = 3
) {
  # TODO: Check for backticks around structure, otherwise error
  # Determine stocks
  stocks <- catch %>% pull({{structure}}) %>% unique
  # Determine prefix
  stock_prefix <- catch %>% select({{structure}}) %>% names()
  # Get number of groups in structure and names
  n_stocks <- tibble(
    Stock = stocks,
    Area = 1:length(stocks),
    Name = paste(stock_prefix, stocks, sep = "")
  )
  # Wrangle data
  res <- catch %>%
    group_by(Year, Period, {{structure}}) %>%
    summarise(Catch = sum(Catch) / ifelse(kilo, 1000, 1)) %>%
    ungroup() %>%
    # TODO: Some of this is just filler
    mutate(Type = 1, Catch = round(Catch, digits = n_digits)) %>%
    rename(Gear = Period, Value = Catch, Stock = {{structure}}) %>%
    full_join(y = n_stocks, by = "Stock") %>%
    select(Year, Gear, Area, Type, Value, Name) %>%
    rename(Stock = Name) %>%
    arrange(Stock, Gear, Year)
  # Return catch
  return(res)
} # End siscah_catch function

#' SISCAH input: spawn data.
#'
#' Make the input file for SISCAH: spawn index data.
#'
#' @template param-spawn
#' @template param-structure
#' @param kilo Logical. Return values in thousands. Default TRUE.
#' @param n_digits Numeric. Number of decimal places for values. Default 3.
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @return Tibble with catch data in thousands aggregated by Year, Period, and
#'   `structure`, including spatial and temporal information.
#' @family SISCAH
#' @export
siscah_spawn <- function(
    spawn, structure = `Region`, kilo = TRUE, n_digits = 3
) {
} # End siscah_spawn function
