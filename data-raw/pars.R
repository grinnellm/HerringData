# R packages
library(tibble)
library(readr)
library(dplyr)
library(here)

# Disposal codes
codes_disposal <- read_csv(
  file = here("data-raw", "Disposal.csv"),
  col_types = cols("i", "c", "c", "i", "i", "c")
)
save(codes_disposal, file = here("data", "codes_disposal.RData"), version = 2)

# Gear codes
codes_gear <- read_csv(
  file = here("data-raw", "Gear.csv"),
  col_types = cols("i", "c", "i", "i", "i")
)
save(codes_gear, file = here("data", "codes_gear.RData"), version = 2)

# Group codes
codes_group <- read_csv(
  file = here("data-raw", "Group.csv"),
  col_types = cols("c", "c")
)
save(codes_group, file = here("data", "codes_group.RData"), version = 2)

# Period codes
codes_period <- read_csv(
  file = here("data-raw", "Period.csv"),
  col_types = cols("c", "c")
)
save(codes_period, file = here("data", "codes_period.RData"), version = 2)

# Source codes
codes_source <- read_csv(
  file = here("data-raw", "Source.csv"),
  col_types = cols("i", "c", "c")
)
save(codes_source, file = here("data", "codes_source.RData"), version = 2)

# Conversion factors
conv_factors <- list(
  st2t = 0.90718474, # Short tons to tonnes
  ft2m = 0.3048, # Feet to metres
  lb2kg = 0.453592 # Pounds to kilograms
)
save(conv_factors, file = here("data", "conv_factors.RData"), version = 2)
