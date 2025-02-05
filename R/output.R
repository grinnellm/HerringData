#' SISCAH input: biological data.
#'
#' Make input data for SISCAH: biological data (number- and weight-at-age).
#'
#' @template param-bio
#' @template param-structure
#' @template param-age_min
#' @template param-age_max
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @return List of tibbles with biological data aggregated by Year, Period, and
#'   "structure", including spatial and temporal information. List has two
#'   tibbles: number-at-age and weight-at-age in grams.
#' @family SISCAH
#' @export
#' @examples
#' require(here)
#' require(tidyverse)
#' bio_raw <- readRDS(file = here("Examples", "bio_raw.rds"))
#' siscah_bio <- bio_raw %>%
#'   filter(Region == "PRD") %>% siscah_bio(structure = "StatArea")
siscah_bio <- function(
    bio,
    structure = "Region",
    age_min = 1,
    age_max = 10,
    n_digits = 3
) {
  # TODO: Check for backticks around structure, otherwise error
  # Determine stocks
  stocks <- bio %>% pull({{structure}}) %>% unique
  # Determine prefix
  stock_prefix <- bio %>% select({{structure}}) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = "")
  )
  # Use 'SampWt' column to fix unrepresentative sampling if identified
  number_aged <- bio %>%
    filter(Age >= age_min) %>%
    mutate(Age = ifelse(Age > age_max, age_max, Age)) %>%  # Plus group
    select(Period, Year, {{structure}}, Age, SampWt) %>%
    na.omit() %>%
    group_by(Period, Year, across(structure), Age) %>%
    summarise(Number = round(sum_na(SampWt))) %>%
    # mutate(Proportion = Number / sum_na(Number)) %>%
    ungroup() %>%
    # TODO: This is just filler
    mutate(Type = 1) %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Gear, Year, Type, Area, Stock, Age, Number)
    # arrange(Period, Year, {{structure}}, Age)
  # Wide format for SISCAH: number aged
  siscah_number_aged <- number_aged %>%
    # tibble() %>%
    # select(Year, Period, Age, Number) %>%
    # mutate(Number = round(Number)) %>%
    pivot_wider(
      names_from = Age, values_from = Number, values_fill = 0,
      names_prefix = "Age"
    ) %>%
    # rename(Gear = Period) %>%
    # mutate_all(as.integer) %>%
    # arrange(Period, Year, {{structure}}) %>%
    # select(Year, Gear, Area, Type, Value, Stock) %>%
    arrange(Stock, Gear, Year)
  # List to return
  res <- list(number_aged = siscah_number_aged)
  # Wrangle data
  # res <- catch %>%
  #   group_by(Year, Period, {{structure}}) %>%
  #   summarise(Catch = sum(Catch) / ifelse(kilo, 1000, 1)) %>%
  #   ungroup() %>%
  #   # TODO: Some of this is just filler
  #   mutate(Type = 1, Catch = round(Catch, digits = n_digits)) %>%
  #   rename(Gear = Period, Value = Catch, Name = {{structure}}) %>%
  #   full_join(y = stock_info, by = "Name") %>%
  #   select(Year, Gear, Area, Type, Value, Stock) %>%
  #   arrange(Stock, Gear, Year)
  # Return catch
  return(res)
} # End siscah_bio function

#' SISCAH input: catch data.
#'
#' Make input data for SISCAH: catch data.
#'
#' @template param-catch
#' @template param-structure
#' @template param-kilo
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @return Tibble with catch data aggregated by Year, Period, and "structure",
#'   including spatial and temporal information.
#' @family SISCAH
#' @export
#' @examples
#' require(here)
#' require(tidyverse)
#' catch_raw <- readRDS(file = here("Examples", "catch_raw.rds"))
#' siscah_catch <- catch_raw %>%
#'   filter(Region == "PRD") %>% siscah_catch(structure = "StatArea")
siscah_catch <- function(
    catch,
    structure = "Region",
    kilo = TRUE,
    n_digits = 3
) {
  # Determine stocks
  stocks <- catch %>% pull({{structure}}) %>% unique
  # Determine prefix for stock name
  stock_prefix <- catch %>% select({{structure}}) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = "")
  )
  # Wrangle data
  res <- catch %>%
    group_by(Year, Period, across(structure)) %>%
    summarise(Catch = sum(Catch) / ifelse(kilo, 1000, 1)) %>%
    ungroup() %>%
    # TODO: Some of this is just filler
    mutate(Type = 1, Catch = round(Catch, digits = n_digits)) %>%
    rename(Gear = Period, Value = Catch, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Year, Gear, Area, Type, Value, Stock) %>%
    arrange(Stock, Gear, Year)
  # Return catch
  return(res)
} # End siscah_catch function

#' SISCAH input: spawn data.
#'
#' Make input data for SISCAH: spawn index data.
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
#'   "structure", including spatial and temporal information.
#' @family SISCAH
#' @export
siscah_spawn <- function(
    spawn, structure = "Region", kilo = TRUE, n_digits = 3
) {
} # End siscah_spawn function
