#' SISCAH input: biological data.
#'
#' Make input data for SISCAH: biological data (number- and weight-at-age).
#'
#' @template param-bio
#' @template param-structure
#' @template param-age_min_number
#' @template param-age_min_weight
#' @template param-age_max
#' @template param-n_roll
#' @template param-kilo_weight
#' @template param-year_start
#' @template param-year_end
#' @template param-secs
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select filter_at any_vars
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider complete
#' @importFrom stats na.omit
#' @importFrom zoo na.fill
#' @return List of tibbles with biological data aggregated by Year, Period, and
#'   "structure", including spatial and temporal information. List has three
#'   tibbles: number-at-age and two with weight-at-age in grams (or kilograms if
#'   `kilo-weight` is `TRUE`.
#' @family SISCAH
#' @export
#' @examples
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' data(undefined_sections)
#' if(file.exists(here("Examples", "bio_raw.rds"))){
#'   bio_raw <- readRDS(file = here("Examples", "bio_raw.rds"))
#' } else{
#'   example(load_bio)
#' }
#' dat <- bio_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_bio(structure = "Section")
siscah_bio <- function(
    bio,
    structure = "Section",
    age_min_number = 1,
    age_min_weight = 2,
    age_max = 10,
    n_roll = 5,
    kilo_weight = TRUE,
    year_start = SpawnIndex::pars$years$assess,
    year_end = 2024,
    secs = undefined_sections,
    n_digits = 4
) {
  # If grouping by Section, check for data in undefined Sections
  if(structure == "Section") {
    check_sections(dat = bio, secs = secs, dat_name = "Biological")
  } # End if Section
  # If CC by Region, warning about unbalanced samples
  if("CC" %in% bio$Region & structure == "Region") {
    warning(
      "Check for unbalanced biosampling in CC (see `?unbalanced_sampling`)",
      call. = FALSE
    )
  } # End if CC by Region
  # Determine stocks
  stocks <- bio %>% pull({{structure}}) %>% unique
  # Determine prefix
  stock_prefix <- bio %>% select(all_of(structure)) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = ".")
  )
  # Fix ages
  bio <- bio %>%
    mutate(Age = ifelse(Age > age_max, age_max, Age))  # Plus group
  # Number-at-age
  number_age <- bio %>%
    filter(Age >= age_min_number) %>%
    select(Period, Year, all_of(structure), Age) %>%
    na.omit() %>%
    group_by(Period, Year, across(structure), Age) %>%
    summarise(Number = n()) %>%
    ungroup() %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Gear, Year, Age, Number) %>%
    arrange(Stock, Area, Gear, Year, Age)
  # Age names for number-at-age
  age_names_number <- paste0("a", age_min_number:age_max)
  # Number-at-age (wide format for SISCAH)
  number_age_siscah <- number_age %>%
    pivot_wider(
      names_from = Age, values_from = Number, values_fill = 0,
      names_prefix = "a"
    ) %>%
    arrange(Stock, Area, Gear, Year) %>%
    select(Stock, Area, Gear, Year, age_names_number)
  # Weight-at-age
  weight_age_gear <- bio %>%
    filter(Age >= age_min_weight) %>%
    select(Period, Year, all_of(structure), Age, Weight) %>%
    na.omit() %>%
    group_by(Period, Year, across(structure), Age) %>%
    summarise(Weight = mean_na(x = Weight) / ifelse(kilo_weight, 1000, 1)) %>%
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
  weight_age_gear <- weight_age_gear %>%
    complete(Stock, Area, Gear, Year, Age)
  # Weight-at-age: fill in missing values
  weight_age_gear_fill <- weight_age_gear %>%
    # pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
    #              names_transform = as.integer) %>%
    group_by(Stock, Area, Gear, Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = mean_na_roll(x = Weight, n = n_roll)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Weight = na.fill(Weight, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    mutate(Weight = round(Weight, digits = n_digits))
  # Age names for weight-at-age
  age_names_weight <- paste0("a", age_min_weight:age_max)
  # Weight-at-age: wide format for SISCAH
  weight_age_gear_siscah <- weight_age_gear_fill %>%
    pivot_wider(
      names_from = Age, values_from = Weight, values_fill = 0,
      names_prefix = "a"
    ) %>%
    filter_at(all_of(age_names_weight), any_vars(!is.na(.))) %>%
    arrange(Stock, Area, Gear, Year) %>%
    select(Stock, Area, Gear, Year, age_names_weight)
  # Weight-at-age (seine)
  weight_age_seine <- bio %>%
    filter(Age >= age_min_weight, GearCode == 29) %>%
    select(Year, all_of(structure), Age, Weight) %>%
    na.omit() %>%
    group_by(Year, across(structure), Age) %>%
    summarise(Weight = mean_na(x = Weight) / ifelse(kilo_weight, 1000, 1)) %>%
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
  weight_age_seine <- weight_age_seine %>%
    complete(Stock, Area, Year, Age)
  # Weight-at-age (seine): fill in missing values
  weight_age_seine_fill <- weight_age_seine %>%
    # pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
    #              names_transform = as.integer) %>%
    group_by(Stock, Area, Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = mean_na_roll(x = Weight, n = n_roll)) %>%
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
    filter_at(all_of(age_names_weight), any_vars(!is.na(.))) %>%
    arrange(Stock, Area, Year) %>%
    select(Stock, Area, Year, age_names_weight)
  # List of tibbles
  res <- list(
    number_age = number_age_siscah,
    weight_age_gear = weight_age_gear_siscah,
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
#' @template param-secs
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @return Tibble with catch data aggregated by Year, Period, and "structure".
#'   Catch is in tonnes for gear 1 (other; reduction, food and bait, as well as
#'   special use), gear 2 (roe seine), and gear 3 (roe gillnet), and in pounds
#'   for gear 4 (spawn on kelp; SOK); values are in thousands if `kilo` is
#'   `TRUE`.
#' @family SISCAH
#' @export
#' @examples
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' data(undefined_sections)
#' if(file.exists(here("Examples", "catch_raw.rds"))){
#'   catch_raw <- readRDS(file = here("Examples", "catch_raw.rds"))
#' } else{
#'   example(load_catch)
#' }
#' dat <- catch_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_catch(structure = "Section")
siscah_catch <- function(
    catch,
    structure = "Section",
    kilo = TRUE,
    secs = undefined_sections,
    n_digits = 3
) {
  # If grouping by Section, check for data in undefined Sections
  if(structure == "Section") {
    check_sections(dat = catch, secs = secs, dat_name = "Catch")
  } # End if Section
  # Determine stocks
  stocks <- catch %>% pull({{structure}}) %>% unique
  # Determine prefix for stock name
  stock_prefix <- catch %>% select(all_of(structure)) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = ".")
  )
  # Wrangle data
  res <- catch %>%
    group_by(Year, Period, across(structure)) %>%
    summarise(Value = sum(Catch) / ifelse(kilo, 1000, 1)) %>%
    ungroup() %>%
    mutate(Value = round(Value, digits = n_digits)) %>%
    rename(Gear = Period, Name = {{structure}}) %>%
    full_join(y = stock_info, by = "Name") %>%
    select(Stock, Area, Gear, Year, Value) %>%
    arrange(Stock, Area, Gear, Year) %>%
    filter(Value != 0)
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
#' @template param-secs
#' @template param-n_digits
#' @importFrom Rdpack reprompt
#' @importFrom SpawnIndex load_width eggs_to_sb calc_surf_index calc_macro_index
#'   calc_under_index load_all_spawn
#' @importFrom dplyr pull group_by summarise ungroup mutate rename full_join
#'   select
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @return Tibble with spawn index aggregated by Year and "structure". Spawn is
#'   in tonnes for surface and dive surveys; values are in thousands if `kilo`
#'   is `TRUE`.
#' @family SISCAH
#' @export
#' @examples
#' library(magrittr)
#' library(here)
#' library(dplyr)
#' data(undefined_sections)
#' if(file.exists(here("Examples", "spawn_raw.rds"))){
#'   spawn_raw <- readRDS(file = here("Examples", "spawn_raw.rds"))
#' } else{
#'   example(load_spawn)
#' }
#' dat <- spawn_raw %>%
#'   filter(Region == "PRD") %>%
#'   siscah_spawn(structure = "Section")
siscah_spawn <- function(
    spawn,
    structure = "Section",
    kilo = TRUE,
    secs = undefined_sections,
    n_digits = 3
) {
  # Tibble
  spawn <- spawn %>%
    tibble()
  # If grouping by Section, check for data in undefined Sections
  if(structure == "Section") {
    check_sections(dat = spawn, secs = secs, dat_name = "Spawn")
  } # End if Section
  # Determine stocks
  stocks <- spawn %>% pull({{structure}}) %>% unique
  # Determine prefix for stock name
  stock_prefix <- spawn %>% select(all_of(structure)) %>% names()
  # Get number of groups in structure and stock names
  stock_info <- tibble(
    Name = stocks[order(stocks)],
    Area = 1:length(stocks),
    Stock = paste(stock_prefix, stocks[order(stocks)], sep = ".")
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
