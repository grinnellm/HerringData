#' SISCAH input: biological data.
#'
#' Make input data for SISCAH: biological data (number- and weight-at-age).
#'
#' @template param-bio
#' @template param-structure
#' @template param-age_min_number
#' @template param-age_min_weight
#' @template param-age_max
#' @template param-kilo_weight
#' @template param-year_start
#' @template param-year_end
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider complete
#' @importFrom stats na.omit
#' @importFrom zoo na.fill
#' @return List of tibbles with biological data aggregated by Year, Period, and
#'   "structure", including spatial and temporal information. List has three
#'   tibbles: number-at-age and two with weight-at-age in grams.
#' @family SISCAH
#' @export
#' @examples
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' if(file.exists(here("Examples", "bio_raw.rds"))){
#'   bio_raw <- readRDS(file = here("Examples", "bio_raw.rds"))
#' } else{
#'   example(load_bio)
#' }
#' dat <- bio_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_bio(structure = "Region")
siscah_bio <- function(
    bio,
    structure = "Region",
    age_min_number = 1,
    age_min_weight = 2,
    age_max = 10,
    kilo_weight = TRUE,
    year_start = SpawnIndex::pars$years$assess,
    year_end = 2024,
    n_digits = 4
) {
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
  # Fix ages
  bio <- bio %>%
    mutate(Age = ifelse(Age > age_max, age_max, Age))  # Plus group
  # Number-at-age (SampWt fixes unrepresentative sampling if identified)
  number_age <- bio %>%
    filter(Age >= age_min_number) %>%
    select(Period, Year, {{structure}}, Age, SampWt) %>%
    na.omit() %>%
    group_by(Period, Year, across(structure), Age) %>%
    summarise(Number = round(sum_na(SampWt))) %>%
    ungroup() %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Gear, Year, Age, Number)
  # Number-at-age (wide format for SISCAH)
  number_age_siscah <- number_age %>%
    pivot_wider(
      names_from = Age, values_from = Number, values_fill = 0,
      names_prefix = "a"
    ) %>%
    arrange(Stock, Area, Gear, Year)
  # Weight-at-age (SampWt fixes unrepresentative sampling if identified)
  weight_age <- bio %>%
    filter(Age >= age_min_weight) %>%
    select(Period, Year, {{structure}}, Age, Weight, SampWt) %>%
    na.omit() %>%
    group_by(Period, Year, across(structure), Age) %>%
    summarise(
      Weight = mean_na_weight(x = Weight, w = SampWt) /
        ifelse(kilo_weight, 1000, 1)
    ) %>%
    ungroup() %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    complete(
      Gear = unique(bio$Period),
      Year = year_start:year_end,
      Name = stock_info$Name,
      Age = age_min_weight:age_max
    ) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Gear, Year, Age, Weight) %>%
    arrange(Stock, Area, Gear, Year, Age)
  # Weight-at-age: fill in missing values
  weight_age_fill <- weight_age %>%
    # pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
    #              names_transform = as.integer) %>%
    group_by(Stock, Area, Gear, Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = mean_na_roll(x = Weight)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Weight = na.fill(Weight, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    mutate(Weight = round(Weight, digits = n_digits))
  # Weight-at-age: wide format for SISCAH
  weight_age_siscah <- weight_age_fill %>%
    pivot_wider(
      names_from = Age, values_from = Weight, values_fill = 0,
      names_prefix = "a"
    ) %>%
    arrange(Stock, Area, Gear, Year)
  # Weight-at-age (seine; SampWt fixes unrepresentative sampling if identified)
  weight_age_seine <- bio %>%
    filter(Age >= age_min_weight, GearCode == 29) %>%
    select(Year, {{structure}}, Age, Weight, SampWt) %>%
    na.omit() %>%
    group_by(Year, across(structure), Age) %>%
    summarise(
      Weight = mean_na_weight(x = Weight, w = SampWt) /
        ifelse(kilo_weight, 1000, 1)
    ) %>%
    ungroup() %>%
    rename(Name = {{structure}}) %>%
    complete(
      Year = year_start:year_end,
      Name = stock_info$Name,
      Age = age_min_weight:age_max
    ) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Year, Age, Weight) %>%
    arrange(Stock, Area, Year, Age)
  # Weight-at-age (seine): fill in missing values
  weight_age_seine_fill <- weight_age_seine %>%
    # pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
    #              names_transform = as.integer) %>%
    group_by(Stock, Area, Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = mean_na_roll(x = Weight)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Weight = na.fill(Weight, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    mutate(Weight = round(Weight, digits = n_digits))
  # Weight-at-age (seine): wide format for SISCAH
  weight_age_seine_siscah <- weight_age_seine_fill %>%
    pivot_wider(
      names_from = Age, values_from = Weight, values_fill = 0,
      names_prefix = "a"
    ) %>%
    arrange(Stock, Area, Year)
  # List of tibbles
  res <- list(
    number_age = number_age_siscah,
    weight_age = weight_age_siscah,
    weight_age_seine = weight_age_seine_siscah
  )
  # Return biodata
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
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' if(file.exists(here("Examples", "catch_raw.rds"))){
#'   catch_raw <- readRDS(file = here("Examples", "catch_raw.rds"))
#' } else{
#'   example(load_catch)
#' }
#' dat <- catch_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_catch(structure = "Region")
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
    mutate(Catch = round(Catch, digits = n_digits)) %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Gear, Year, Catch) %>%
    arrange(Stock, Area, Gear, Year)
  # Return catch
  return(res)
} # End siscah_catch function

#' SISCAH input: spawn data.
#'
#' Make input data for SISCAH: spawn index data.
#'
#' @template param-spawn
#' @template param-structure
#' @template param-kilo
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom SpawnIndex load_width eggs_to_sb calc_surf_index calc_macro_index
#'   calc_under_index load_all_spawn
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @return Tibble with catch data in thousands aggregated by Year, Period, and
#'   "structure", including spatial and temporal information.
#' @family SISCAH
#' @export
#' @examples
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' if(file.exists(here("Examples", "spawn_raw.rds"))){
#'   spawn_raw <- readRDS(file = here("Examples", "spawn_raw.rds"))
#' } else{
#'   example(load_spawn)
#' }
#' dat <- spawn_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_spawn(structure = "Region")
siscah_spawn <- function(
    spawn,
    structure = "Region",
    kilo = TRUE,
    n_digits = 3
) {
  # Tibble
  spawn <- spawn %>%
    tibble()
  # Determine stocks
  stocks <- spawn %>% pull({{structure}}) %>% unique
  # Determine prefix for stock name
  stock_prefix <- spawn %>% select({{structure}}) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = "")
  )
  # Wrangle data
  res <- spawn %>%
    group_by(Year, across(structure)) %>%
    summarise(
      Surface = sum_na(SurfSI) / ifelse(kilo, 1000, 1),
      Macro = sum_na(MacroSI) / ifelse(kilo, 1000, 1),
      Under = sum_na(UnderSI) / ifelse(kilo, 1000, 1)
      ) %>%
    ungroup() %>%
    replace_na(replace = list(Surface = 0, Macro = 0, Under = 0)) %>%
    mutate(
      Surface = round(Surface, digits = n_digits),
      Dive = round(Macro + Under, digits = n_digits)
      ) %>%
    rename(Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Year, Surface, Dive) %>%
    arrange(Stock, Area, Year)
  # Return spawn
  return(res)
} # End siscah_spawn function
