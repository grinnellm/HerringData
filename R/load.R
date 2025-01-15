#' Load catch data from the database.
#'
#' Load and wrangle catch data from the database.
#'
#' @template param-db_conn
#' @template param-db_location
#' @template param-area_table
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @importFrom SpawnIndex load_area_data
#' @importFrom dplyr %>% distinct select mutate left_join group_by summarise
#'   ungroup filter rename n bind_rows arrange
#' @importFrom tibble tibble
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @importFrom lubridate date
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
#' catch_loc <- list(
#'   schema = "Catch",
#'   tables = list(
#'     tCatch = "tCatchData", hCatch = "HailCatch", sokCatch = "SpawnOnKelp"
#'   ),
#'   columns = list(
#'     tCatch = c(
#'       "Season", "LocationCode","GearCode", "DisposalCode", "Catch", "Date"
#'     ),
#'     hCatch = c(
#'       "Active", "Season", "Section", "GearCode", "DisposalCode", "CatchTons"
#'     ),
#'     sokCatch = c(
#'       "Season", "Section", "GearCode", "DisposalCode", "ProductLanded"
#'     )
#'   )
#' )
#' areas <- load_area_data(
#'   db = herring_conn, where = area_loc, reg = "CC", sec_sub = NULL,
#'   groups = codes_group
#' )
#' catch_raw <- load_catch(
#'   db_conn = herring_conn, db_location = catch_loc, area_table = areas
#' )
load_catch <- function(
    db_conn,
    db_location,
    area_table,
    quiet = FALSE
) {
  # Progress message
  if(!quiet) cat("Loading catch data... ")
  # Establish database connection
  cnn <- dbConnect(
    odbc(),
    Driver = db_conn$driver,
    Server = db_conn$server,
    Database = db_conn$database,
    Trusted_Connection = db_conn$trusted
  )
  # SQL query
  sql_t <- paste(
    "SELECT", paste(db_location$columns$tCatch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$tCatch, sep = ".")
  )
  # Access the tCatch worksheet
  tCatch <- dbGetQuery(conn = cnn, statement = sql_t)
  # Error if data was not fetched
  if (!is.data.frame(tCatch)) {
    stop("No data available in MS Access connection: tCatch")
  }
  # Wrangle areas
  areas <- area_table %>%
    tibble() %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName
    ) %>%
    distinct()
  # Wrangle catch
  tCatch <- tCatch %>%
    mutate(
      Year = season_to_year(Season), Source = rep("Tab", times = n()),
      Date = date(Date)
    ) %>%
    left_join(y = areas, by = "LocationCode") %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = sum_nA(Catch)) %>%
    ungroup()
  # SQL query
  sql_h <- paste(
    "SELECT", paste(db_location$columns$hCatch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$hCatch, sep = ".")
  )
  # Access the hail worksheet
  hCatch <- dbGetQuery(conn = cnn, statement = sql_h)
  # Error if data was not fetched
  if (!is.data.frame(hCatch)) {
    stop("No data available in MS Access connection: hCatch")
  }
  # Wrangle catch
  hCatch <- hCatch %>%
    mutate(Section = formatC(Section, width=3, format="d", flag="0")) %>%
    filter(Active == 1) %>%
    mutate(
      Year = season_to_year(Season), Catch = CatchTons * conv_factors$st2t,
      Source = rep("Hail", times = n()), Date = as.Date(NA)
    ) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = sum_na(Catch)) %>%
    ungroup()
  # SQL query
  sql_sok <- paste(
    "SELECT", paste(db_location$columns$sokCatch, collapse = ", "),
    "FROM", paste(db_location$schema, db_location$tables$sokCatch, sep = ".")
  )
  # Access the sok worksheet
  sokCatch <- dbGetQuery(conn = cnn, statement = sql_sok)
  # Error if data was not fetched
  if (!is.data.frame(sokCatch)) {
    stop("No data available in MS Access connection: sokCatch")
  }
  # Wrangle sok
  sokCatch <- sokCatch %>%
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
  allCatch <- bind_rows(tCatch, hCatch, sokCatch)
  # Smaller subset of area information
  areasSm <- areas %>%
    select(SAR, Region, RegionName, StatArea, Group, Section) %>%
    distinct()
  # Merge with area information and determine Period
  res <- allCatch %>%
    left_join(y = areasSm, by = "Section") %>%
    mutate(
      Period = 0,
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(1, 3, 4, 5, 6), 1, Period),
      Period = ifelse(Source == "Hail" & DisposalCode %in% c(3, 6), 1, Period),
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(7, 8) &
                        GearCode %in% c(29), 2, Period),
      Period = ifelse(Source == "Tab" & DisposalCode %in% c(7, 8) &
                        GearCode %in% c(19), 3, Period)
    ) %>%
    filter(Period != 0, !is.na(Region), !is.na(StatArea), !is.na(Section)) %>%
    select(
      Year, Period, Region, StatArea, Section, GearCode, DisposalCode, Date,
      Catch
    ) %>%
    arrange(
      Year, Period, Region, StatArea, Section, GearCode, DisposalCode, Date
    )
  # Close the connection
  dbDisconnect(conn = cnn)
  # TODO: Save data for next time
  # saveRDS(res, file = here("..", "Data", "Raw", "CatchRaw.rds"))
  # Update progress message
  if(!quiet) cat("done\n")
  # Return the data
  return(res)
} # End load_catch function
