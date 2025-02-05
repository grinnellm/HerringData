#' Transform season to year.
#'
#' Transform season to year. The herring 'season' column is a combination of the
#' two fishery years: for example, season '20123' indicates the years 2012 and
#' 2013. The input *.dat file for the analysis using ADMB requires this to be an
#' actual year, which we define as the second (i.e., later) year, 2013. This
#' function takes in a vector of seasons (dat), and outputs a vector of years
#' (res).
#'
#' @param dat Numeric. Values to transform.
season_to_year <- function(dat) {
  # Grab the first 4 characters
  chars <- substr(x = dat, start = 1, stop = 4)
  # Convert to numeric
  digits <- as.numeric(x = chars)
  # Add one to get the year
  res <- digits + 1
  # Return years (as an integer)
  return(as.integer(res))
} # End season_to_year function

#' Calculate sum.
#'
#' Calculate sum if there are non-NA values, return NA if all values are NA.
#' This does not fail when groups have no values to summarise.
#'
#' @param x Numeric. Values to sum.
#' @param omit_na Logical. Keep or remove NAs.
sum_na <- function(x, omit_na = TRUE) {
  # An alternate version to sum(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the sum.
  # If all NA, NA; otherwise, sum
  ifelse(all(is.na(x)),
         res <- NA,
         res <- sum(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End SumNA function
