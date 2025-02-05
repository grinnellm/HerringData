#' Load biological data.
#'
#' Load and wrangle biological data from the database.
#'
#' @template param-db_conn
#' @template param-db_location
#' @template param-area_table
#' @template param-year_start
#' @param rep_year_start Year indicating the start of representative samples. Default 2014.
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom SpawnIndex load_area_data
#' @importFrom dplyr %>% distinct select mutate left_join group_by summarise
#'   ungroup filter rename n bind_rows arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @importFrom lubridate date
#' @return Tibble with biological data (e.g., length, weight, age, sex)
#'   including spatial and temporal information.
#' @family loaders
#' @export
#' @examples
#' library(SpawnIndex)
#' library(DBI)
#' data(regions)
#' data(pars)
#' data(codes_group)
#' data(conv_factors)
#' herring_conn <- list(
#'   driver = "SQL Server Native Client 11.0",
#'   server = "DFBCV9TWVASP001\\SQLEXPRESS16",
#'   database = "Herring",
#'   trusted = "Yes"
#' )
#' area_loc <- list(
#'   schema = "Location",
#'   tables = list(sections = "Sections", locations = "Location"),
#'   columns = list(
#'     sections = c("SAR", "Section"),
#'     locations = c(
#'       "Loc_Code", "Location", "StatArea", "Section", "Bed",
#'        "Location_Latitude", "Location_Longitude")
#'   )
#' )
#' bio_loc <- list(
#'   schema = "Biosample",
#'   tables = list(samples = "sample", fish = "fish"),
#'   columns = list(
#'     samples = c(
#'       "season", "loc_code", "Set_Longitude", "Set_Latitude", "isamp",
#'       "month", "Representative_Set", "source_code", "gear_code"
#'     ),
#'     fish = c(
#'       "season", "isamp", "fish", "len", "wgt", "sex_alpha", "mat_code",
#'       "age", "dual_age", "gonad_len", "gonad_wgt"
#'     )
#'   )
#' )
#' areas <- load_area_data(
#'   db = herring_conn, where = area_loc, reg = "All", groups = codes_group,
#'   quiet = TRUE
#' )
#' bio_raw <- load_bio(
#'   db_conn = herring_conn, db_location = bio_loc, area_table = areas
#' )
load_bio <- function(
    db_conn,
    db_location,
    area_table,
    year_start = SpawnIndex::pars$years$assess,
    rep_year_start = 2014,
    quiet = FALSE
) {
  # Progress message
  if(!quiet) cat("Loading biological data... ")
  # Establish database connection
  db_connection <- dbConnect(
    odbc::odbc(), Driver = db_conn$driver, Server = db_conn$server,
    Database = db_conn$database, Trusted_Connection = db_conn$trusted
  )
  # Wrangle areas
  area_table <- area_table %>%
    tibble() %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName
    ) %>%
    distinct()
  # SQL query: samples
  sql_samples <- paste(
    "SELECT", paste(db_location$columns$sample, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$samples, sep = ".")
  )
  # Access the sample worksheet
  sample_dat <- dbGetQuery(conn = db_connection, statement = sql_samples)
  # Error if data was not fetched
  if (!is.data.frame(sample_dat)) {
    stop("Data not available: sample_dat")
  }
  # # Grab the spatial info and process
  # sampleSP <- sample_dat %>%
  #   transmute(
  #     X = ifelse(is.na(Set_Longitude), 0, Set_Longitude),
  #     Y = ifelse(is.na(Set_Latitude), 0, Set_Latitude)
  #   )
  # # Put X and Y into a spatial points object
  # sPts <- SpatialPoints(coords = sampleSP, proj4string = CRS(inCRS))
  # # Save the original points
  # sPtsOrig <- as_tibble(sPts) %>%
  #   rename(Longitude = X, Latitude = Y)
  # # Convert X and Y from WGS to Albers
  # sPtsAlb <- spTransform(x = sPts, CRSobj = CRS(outCRS))
  # # Extract spatial info
  # dfAlb <- as_tibble(sPtsAlb)
  # Extract relevant sample data
  samples <- sample_dat %>%
    # cbind(dfAlb) %>%
    # cbind(sPtsOrig) %>%
    rename(
      LocationCode = loc_code, Longitude = Set_Longitude,
      Latitude = Set_Latitude, Sample = isamp, Month = month,
      Representative = Representative_Set, SourceCode = source_code,
      GearCode = gear_code
    ) %>%
    mutate(
      Year = season_to_year(season)
      # Eastings = ifelse(is.na(Set_Longitude), Set_Longitude, X),
      # Northings = ifelse(is.na(Set_Latitude), Set_Latitude, Y)
    ) %>%
    select(
      Year, Month, Sample, Representative, LocationCode, Longitude, Latitude,
      SourceCode, GearCode
    ) %>%
    as_tibble()
  # SQL query: fish
  sql_fish <- paste(
    "SELECT", paste(db_location$columns$fish, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$fish, sep = ".")
  )
  # Access the fish worksheet
  fish <- dbGetQuery(conn = db_connection, statement = sql_fish)
  # Error if data was not fetched
  if (!is.data.frame(fish)) {
    stop("Data not available: fish")
  }
  # Wrangle biosamples
  fish <- fish %>%
    rename(
      Sample = isamp, Fish = fish, Length = len, Weight = wgt, Sex = sex_alpha,
      MaturityCode = mat_code, Age = age, DualAge = dual_age,
      GonadLength = gonad_len, GonadWeight = gonad_wgt
    ) %>%
    mutate(
      Year = season_to_year(season),
      # TODO: Move this to the function that makes the output file
      # Age = ifelse(age <= max(ageRange), age, max(ageRange))
    ) %>%
    # TODO: Move this to the function that makes the output file
    # filter(Age >= min(ageRange)) %>%
    select(
      Year, Sample, Fish, Length, Weight, Sex, MaturityCode, Age, DualAge,
      GonadLength, GonadWeight
    ) %>%
    as_tibble()
  # Combine the two tables (note that Sample re-starts at 1 each Year)
  fish_samples <- full_join(x = samples, y = fish, by = c("Year", "Sample"))
  # More wrangling: filter to region(s)
  # TODO: Need to fill in these missing lat/longs (in the database)
  raw <- fish_samples %>%
    # filter(LocationCode %in% areas$LocationCode) %>%
    left_join(y = area_table, by = "LocationCode") %>%
    # mutate(
    # Eastings = ifelse(is.na(Eastings.x), Eastings.y, Eastings.x),
    # Northings = ifelse(is.na(Northings.x), Northings.y, Northings.x),
    # Longitude = ifelse(Longitude.x == 0, Longitude.y, Longitude.x),
    # Latitude = ifelse(Latitude.x == 0, Latitude.y, Latitude.x)
    # ) %>%
    select(
      Year, Month, Region, StatArea, Group, Section, LocationCode,
      LocationName, Longitude, Latitude, Sample, Representative, SourceCode,
      GearCode, Fish, Length, Weight, Sex, MaturityCode, Age, DualAge,
      GonadLength, GonadWeight
    ) %>%
    arrange(
      Year, Month, Region, StatArea, Group, Section, LocationCode, Sample, Fish
    )
  # # Clip the extent
  # df <- ClipExtent(
  #   dat = raw, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  # # Subset data with 'good' X and Y
  # dfNotNA <- df %>%
  #   filter(!is.na(Eastings) & !is.na(Northings))
  # # Subset data with 'bad' X or Y, and try to fill in using transect X and Y
  # dfNA <- df %>%
  #   filter(is.na(Eastings) | is.na(Northings)) %>%
  #   select(-Eastings, -Northings) %>%
  #   left_join(y = XY, by = "LocationCode")
  # # Re-combine the two subsets
  # df2 <- bind_rows(dfNotNA, dfNA)
  # # Clip the extent (again)
  # res <- ClipExtent(
  #   dat = df2, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  res <- raw #%>%
  #   left_join(y = XY, by = "LocationCode") %>%
  #   mutate(
  #     Longitude = ifelse(is.na(Longitude.x), Longitude.y, Longitude.x),
  #     Latitude = ifelse(is.na(Latitude.x), Latitude.y, Latitude.x) ) %>%
  #   select(-Longitude.x, -Longitude.y, -Latitude.x, -Latitude.y) %>%
  #   st_as_sf(coords = c("Longitude", "Latitude"), na.fail = FALSE)
  # Get locations with missing X or Y
  # no_xy <- res %>%
  #   filter(is.na(geometry)) %>%
  #   select(Region, StatArea, Group, Section, LocationCode, LocationName) %>%
  #   distinct()
  # Message re missing X and Y, if any
  # if (nrow(no_xy) >= 1) {
  #   warning("There are ", nrow(no_xy),
  #           " biological sample location(s) with missing or incorrect spatial info",
  #           call. = FALSE
  #   )
  # }
  # Stop if we're missing rows
  # if (nrow(raw) != nrow(res)) stop("Missing rows!", call. = FALSE)
  # Warning if more recent data is available
  # if (max(res$Year, na.rm = TRUE) > max(yrRange)) {
  #   warning("Recent biological data exists; update 'yrRange' to include ",
  #           paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse = ", "),
  #           call. = FALSE
  #   )
  # }
  # Trim years outside the desired year range
  # TODO: Move this to the function that makes the output file
  # res <- res %>%
  #   filter(Year %in% yrRange)
  # Determine period and choose representative samples
  res <- res %>%
    mutate(
      Period = 0,
      Period = ifelse(GearCode == 29 & SourceCode %in% c(1, 6, 7),
                      1, Period),
      # If include test seine, source %in% c(0, 5), else source == 0
      Period = ifelse(GearCode == 29 & SourceCode %in% c(0, 5),
                      2, Period),
      # SOK data
      Period = ifelse(GearCode == 29 & SourceCode == 4 & Month %in% c(3, 4),
                      2, Period),
      # If include test gillnet, source %in% c(0, 5), else source == 0
      Period = ifelse(GearCode == 19 & SourceCode == 0,
                      3, Period),
      SampWt = 1
    ) %>%
    filter(Period != 0, Year >= year_start) %>%
    # TODO: Fix non-representative sampling here

    # TODO: Need a better way to do this
    # Keep all prior to rep_year_start and representative ones since then
    filter(Year < rep_year_start | Representative == 1) %>%
    select(-Representative)
  # Close the connection
  dbDisconnect(conn = db_connection)
  # TODO: Save data for next time
  # saveRDS(res, file = here("..", "Data", "Raw", "BioRaw.rds"))
  # Update progress message
  if(!quiet) cat("done\n")
  # Return the data
  return(res)
} # End load_bio function

#' Load catch data.
#'
#' Load and wrangle catch data from the database.
#'
#' @template param-db_conn
#' @template param-db_location
#' @template param-area_table
#' @template param-year_start
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom SpawnIndex load_area_data
#' @importFrom dplyr %>% distinct select mutate left_join group_by summarise
#'   ungroup filter rename n bind_rows arrange
#' @importFrom tibble tibble
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @importFrom lubridate date
#' @return Tibble with catch data including spatial and temporal information.
#' @family loaders
#' @export
#' @examples
#' library(SpawnIndex)
#' library(DBI)
#' data(regions)
#' data(pars)
#' data(codes_group)
#' data(conv_factors)
#' herring_conn <- list(
#'   driver = "SQL Server Native Client 11.0",
#'   server = "DFBCV9TWVASP001\\SQLEXPRESS16",
#'   database = "Herring",
#'   trusted = "Yes"
#' )
#' area_loc <- list(
#'   schema = "Location",
#'   tables = list(sections = "Sections", locations = "Location"),
#'   columns = list(
#'     sections = c("SAR", "Section"),
#'     locations = c(
#'       "Loc_Code", "Location", "StatArea", "Section", "Bed",
#'        "Location_Latitude", "Location_Longitude")
#'   )
#' )
#' areas <- load_area_data(
#'   db = herring_conn, where = area_loc, reg = "All", groups = codes_group,
#'   quiet = TRUE
#' )
#' catch_loc <- list(
#'   schema = "Catch",
#'   tables = list(
#'     tab_catch = "tCatchData", hail_catch = "HailCatch",
#'     sok_catch = "SpawnOnKelp"
#'   ),
#'   columns = list(
#'     tab_catch = c(
#'       "Season", "LocationCode","GearCode", "DisposalCode", "Catch", "Date"
#'     ),
#'     hail_catch = c(
#'       "Active", "Season", "Section", "GearCode", "DisposalCode", "CatchTons"
#'     ),
#'     sok_catch = c(
#'       "Season", "Section", "GearCode", "DisposalCode", "ProductLanded"
#'     )
#'   )
#' )
#' catch_raw <- load_catch(
#'   db_conn = herring_conn, db_location = catch_loc, area_table = areas
#' )
load_catch <- function(
    db_conn,
    db_location,
    area_table,
    year_start = SpawnIndex::pars$years$assess,
    quiet = FALSE
) {
  # Progress message
  if(!quiet) cat("Loading catch data... ")
  # Establish database connection
  db_connection <- dbConnect(
    odbc(), Driver = db_conn$driver, Server = db_conn$server,
    Database = db_conn$database, Trusted_Connection = db_conn$trusted
  )
  # Wrangle areas
  area_table <- area_table %>%
    tibble() %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName
    ) %>%
    distinct()
  # SQL query: tab catch
  sql_tab <- paste(
    "SELECT", paste(db_location$columns$tab_catch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$tab_catch, sep = ".")
  )
  # Access the tab catch worksheet
  tab_catch <- dbGetQuery(conn = db_connection, statement = sql_tab)
  # Error if data was not fetched
  if (!is.data.frame(tab_catch)) {
    stop("Data not available: tab_catch")
  }
  # Wrangle catch
  tab_catch <- tab_catch %>%
    mutate(
      Year = season_to_year(Season), Source = rep("Tab", times = n()),
      Date = date(Date)
    ) %>%
    left_join(y = area_table, by = "LocationCode") %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = sum_na(Catch)) %>%
    ungroup()
  # SQL query: hail catch
  sql_hail <- paste(
    "SELECT", paste(db_location$columns$hail_catch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$hail_catch, sep = ".")
  )
  # Access the hail worksheet
  hail_catch <- dbGetQuery(conn = db_connection, statement = sql_hail)
  # Error if data was not fetched
  if (!is.data.frame(hail_catch)) {
    stop("Data not available: hail_catch")
  }
  # Wrangle hail catch
  hail_catch <- hail_catch %>%
    mutate(Section = formatC(Section, width=3, format="d", flag="0")) %>%
    filter(Active == 1) %>%
    mutate(
      Year = season_to_year(Season), Catch = CatchTons * conv_factors$st2t,
      Source = rep("Hail", times = n()), Date = as.Date(NA)
    ) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = sum_na(Catch)) %>%
    ungroup()
  # SQL query: SOK catch
  sql_sok <- paste(
    "SELECT", paste(db_location$columns$sok_catch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$sok_catch, sep = ".")
  )
  # Access the SOK worksheet
  sok_catch <- dbGetQuery(conn = db_connection, statement = sql_sok)
  # Error if data was not fetched
  if (!is.data.frame(sok_catch)) {
    stop("Data not available: sok_catch")
  }
  # Wrangle SOK catch
  sok_catch <- sok_catch %>%
    mutate(
      Year = season_to_year(Season), Source = rep("SOK", times = n()),
      Date = as.Date(NA),
      Section = formatC(Section, width=3, format="d", flag="0")
    ) %>%
    rename(Catch = ProductLanded) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = sum_na(Catch)) %>%
    ungroup()
  # Combine the three tables
  all_catch <- bind_rows(tab_catch, hail_catch, sok_catch)
  # Smaller subset of area information
  area_table_small <- area_table %>%
    select(SAR, Region, RegionName, StatArea, Group, Section) %>%
    distinct()
  # Merge with area information and determine Period
  res <- all_catch %>%
    left_join(y = area_table_small, by = "Section") %>%
    mutate(
      Period = 0,
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(1, 3, 4, 5, 6),
                      1, Period),
      Period = ifelse(Source == "Hail" & DisposalCode %in% c(3, 6),
                      1, Period),
      # If include test catch, disposal %in% c(7, 8), else disposal == 8
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(7, 8) &
                        GearCode %in% c(29),
                      2, Period),
      # If include test catch, disposal %in% c(7, 8), else disposal == 8
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(7, 8) &
                        GearCode %in% c(19),
                      3, Period)
    ) %>%
    # TODO: Filter by year_start too
    filter(Period != 0) %>%
    select(
      Year, Period, Region, StatArea, Section, GearCode, DisposalCode, Date,
      Catch
    ) %>%
    arrange(
      Year, Period, Region, StatArea, Section, GearCode, DisposalCode, Date
    )
  # Close the connection
  dbDisconnect(conn = db_connection)
  # TODO: Save data for next time
  # saveRDS(res, file = here("..", "Data", "Raw", "CatchRaw.rds"))
  # Update progress message
  if(!quiet) cat("done\n")
  # Return the data
  return(res)
} # End load_catch function
