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
#' @importFrom Rdpack reprompt
#' @return Numeric year.
#' @family utility
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
#' @template param-omit_na
#' @importFrom Rdpack reprompt
#' @return Numeric sum.
#' @family utility
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
} # End sum_na function

#' Calculate mean.
#'
#' Calculate mean if there are non-NA values, return NA if all values are NA.
#' This does not fail when groups have no values to summarise.
#'
#' @param x Numeric. Values to mean.
#' @template param-omit_na
#' @importFrom Rdpack reprompt
#' @return Numeric mean.
#' @family utility
mean_na <- function(x, omit_na = TRUE) {
  # An alternate version to mean(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the mean.
  # If all NA, NA; otherwise, mean
  ifelse(all(is.na(x)),
         res <- NA,
         res <- mean(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End mean_na function

#' Calculate weighted mean.
#'
#' Calculate weighted mean if there are non-NA values, return NA if all values
#' are NA. This does not fail when groups have no values to summarise.
#'
#' @param x Numeric. Values to mean.
#' @param w Numeric. Weights.
#' @template param-omit_na
#' @importFrom Rdpack reprompt
#' @importFrom stats weighted.mean
#' @return Numberic weighted mean.
#' @family utility
weighted_mean_na <- function(x, w, omit_na = TRUE) {
  # An alternate version to weighted.mean(x, w, na.rm=TRUE), which returns 0 if
  # x is all NA. This version retuns NA if x is all NA, otherwise it returns the
  # weighted mean.
  # If all NA, NA; otherwise, weighted mean
  ifelse(all(is.na(x)),
         res <- NA,
         res <- weighted.mean(x, w, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End weighted_mean_na function

#' Calculate rolling mean.
#'
#' Calculate rolling mean of previous values.
#'
#' @param x Numeric. Values to mean.
#' @param n Numeric. Number of values in rolling window (maximum). Default is 5.
#' @importFrom Rdpack reprompt
#' @return Numberic rolling mean.
#' @family utility
rolling_mean_na <- function(x, n = 5) {
  # Update the NAs in a vector with the mean of the previous values. The number
  # of values used can be less than n for NAs near the start of the vector, and
  # will be a maximum of n for values further along the vector. The value will
  # remain NA if no non-NA values are available.
  # Loop over observations starting with the second observation
  for (i in 2:length(x)) {
    # If the value is NA
    if (is.na(x[i])) {
      # Get window for current index: up to n previous values
      mu_window <- (i - min(n, i - 1)):(i - 1)
      # Calculate the mean of the values in the rolling window
      x[i] <- mean_na(x[mu_window])
    } # End if value is NA
  } # End i loop over observations
  # Return the observations with NAs (mostly) replaced by the rolling mean
  return(x)
} # End rolling_mean_na function
