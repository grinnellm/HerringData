% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{sum_na}
\alias{sum_na}
\title{Calculate sum.}
\usage{
sum_na(x, omit_na = TRUE)
}
\arguments{
\item{x}{Numeric. Values to sum.}

\item{omit_na}{Logical. Keep or remove NAs. Default is TRUE.}
}
\value{
Numeric sum.
}
\description{
Calculate sum if there are non-NA values, return NA if all values are NA.
This does not fail when groups have no values to summarise.
}
\seealso{
Other utility: 
\code{\link{mean_na}()},
\code{\link{mean_na_roll}()},
\code{\link{mean_na_weight}()},
\code{\link{season_to_year}()}
}
\concept{utility}
