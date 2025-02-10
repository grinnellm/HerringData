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

# Database information
database_info <- list(
  herring_conn = list(
    driver = "SQL Server Native Client 11.0",
    server = "DFBCV9TWVASP001\\SQLEXPRESS16",
    database = "Herring",
    trusted = "Yes"
  ),
  area_loc = list(
    schema = "Location",
    tables = list(sections = "Sections", locations = "Location"),
    columns = list(
      sections = c("SAR", "Section"),
      locations = c(
        "Loc_Code", "Location", "StatArea", "Section", "Bed",
        "Location_Latitude", "Location_Longitude")
    )
  ),
  bio_loc = list(
    schema = "Biosample",
    tables = list(samples = "sample", fish = "fish"),
    columns = list(
      samples = c(
        "season", "loc_code", "Set_Longitude", "Set_Latitude", "isamp", "month",
        "Representative_Set", "source_code", "gear_code"
      ),
      fish = c(
        "season", "isamp", "fish", "len", "wgt", "sex_alpha", "mat_code",
        "age", "dual_age", "gonad_len", "gonad_wgt"
      )
    )
  ),
  catch_loc = list(
    schema = "Catch",
    tables = list(
      tab_catch = "tCatchData", hail_catch = "HailCatch",
      sok_catch = "SpawnOnKelp"
    ),
    columns = list(
      tab_catch = c(
        "Season", "LocationCode","GearCode", "DisposalCode", "Catch", "Date"
      ),
      hail_catch = c(
        "Active", "Season", "Section", "GearCode", "DisposalCode", "CatchTons"
      ),
      sok_catch = c(
        "Season", "Section", "GearCode", "DisposalCode", "ProductLanded"
      )
    )
  ),
  width_loc = list(
    schema = "Location",
    tables = list(
      region_std = "RegionStd", section_std = "SectionStd", pool_std = "PoolStd"
    ),
    columns = list(
      region_std = c("REGION", "WIDMED"),
      section_std = c("SECTION", "WIDMED"),
      pool_std = c("SECTION", "BED", "WIDMED")
    )
  ),
  surf_loc = list(
    schema = "Spawn",
    tables = list(surface = "Surface", all_spawn = "AllSpawn"),
    columns = list(
      surface = c(
        "Loc_Code", "Spawn_Number", "Year", "Lay_Grass", "Grass_Percent",
        "Lay_Rockweed", "Rockweed_Percent", "Lay_Kelp", "Kelp_Percent",
        "Lay_Brown_Algae", "Brown_Algae_Percent", "Lay_Leafy_Red",
        "Leafy_Red_Percent", "Lay_Stringy_Red", "Stringy_Red_Percent",
        "Lay_Rock", "Rock_Percent", "Lay_Other", "Other_Percent", "Intensity"
      ),
      all_spawn = c(
        "Loc_Code", "Spawn_Number", "Width", "Method", "Year", "Length"
      )
    )
  ),
  macro_loc = list(
    schema = "Spawn",
    tables = list(
      all_spawn = "AllSpawn", plants = "macplantNEW", transects = "MacTrans",
      transects_2024 = "MacTrans2024"
    ),
    columns = list(
      all_spawn = c(
        "Loc_Code", "Spawn_Number", "Length_Macrocystis", "Method", "Year",
        "Length"
      ),
      plants = c("Loc_Code", "Spawn_Number", "Year", "Transect", "Mature"),
      transects = c(
        "Loc_Code", "Spawn_Number", "Transect", "Year", "Height", "Width",
        "Layers"
      )
    )
  ),
  under_loc = list(
    schema = "Spawn",
    tables = list(
      all_spawn = "AllSpawn", alg_trans = "vegtransNEW", stations = "Stations",
      algae = "vegetationNEW", stations_2024 = "stations2024",
      algae_2024 = "Vegetation2024"
    ),
    columns = list(
      all_spawn = c(
        "Loc_Code", "Spawn_Number", "Length_Vegetation", "Method", "Year",
        "Length"
      ),
      alg_trans = c(
        "Year", "Loc_Code", "Spawn_Number", "Quadrat_Size", "Width_Recorded",
        "Transect"
      ),
      stations = c(
        "Loc_Code", "Spawn_Number", "Layers_Bottom", "Year", "Percent_Bottom",
        "Transect", "Station"
      ),
      algae = c(
        "Loc_Code", "Spawn_Number", "Type_Vegetation", "Layers_Vegetation",
        "Year", "Percent_Vegetation", "Transect", "Station"
      )
    )
  ),
  all_loc = list(
    schema = "Spawn",
    tables = list(all_spawn = "Allspawn", stations = "Stations"),
    columns = list(
      all_spawn = c(
        "Loc_Code", "Spawn_Number", "Start", "[End]", "Method", "Year",
        "Length", "Width"
      ),
      stations = c("Loc_Code", "Spawn_Number", "Depth", "Year")
    )
  )
)
save(database_info, file = here("data", "database_info.RData"), version = 2)
