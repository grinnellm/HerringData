% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HerringData.R
\docType{package}
\name{HerringData-package}
\alias{HerringData}
\alias{HerringData-package}
\title{HerringData: Load and wrangle Pacific Herring (\emph{Clupea pallasii}) data.}
\description{
Load Pacific Herring (\emph{Clupea pallasii}) data from databases and
wrangle for data summary reports and input to statistical catch-age models.
Data includes biological (number- and weight-at-age), catch, and spawn
index data.
}
\details{
The HerringData package provides data, as well as three families of
functions: loaders, SISCAH, and utility.
}
\section{Data}{
 The data are disposal codes (\code{\link{codes_disposal}}),
gear codes (\code{\link{codes_gear}}), group codes
(\code{\link{codes_group}}), period codes (\code{\link{codes_period}}),
source codes (\code{\link{codes_source}}), conversion factors
(\code{\link{conv_factors}}) and database information
(\code{\link{database_info}}).
}

\section{Loaders functions}{
 The load functions are \code{\link{load_bio}},
\code{\link{load_catch}}, and \code{\link{load_spawn}}.
}

\section{SISCAH functions}{
 The SISCAH functions are \code{\link{siscah_bio}},
\code{\link{siscah_catch}}, and \code{\link{siscah_spawn}}.
}

\section{Utility functions}{
 The utility function are \code{\link{mean_na}},
\code{\link{mean_na_roll}}, \code{\link{mean_na_weight}},
\code{\link{season_to_year}}, and \code{\link{sum_na}}.
}

\author{
\strong{Maintainer}: Matthew H Grinnell \email{matthew.grinnell@dfo-mpo.gc.ca}

}
\keyword{internal}
