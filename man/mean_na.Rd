% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{mean_na}
\alias{mean_na}
\title{Calculate mean.}
\usage{
mean_na(x, omit_na = TRUE)
}
\arguments{
\item{x}{Numeric. Values to mean.}

\item{omit_na}{Logical. Keep or remove NAs. Default is TRUE.}
}
\value{
Numeric mean.
}
\description{
Calculate mean if there are non-NA values, return NA if all values are NA.
This does not fail when groups have no values to summarise.
}
\seealso{
Other utility: 
\code{\link{mean_na_roll}()},
\code{\link{mean_na_weight}()},
\code{\link{season_to_year}()},
\code{\link{sum_na}()}
}
\concept{utility}
