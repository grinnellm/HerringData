#' Cross-walk table for disposal codes.
#'
#' Cross-walk table with disposal codes and definitions.
#'
#' @format Tibble with rows for disposal codes and columns for definitions.
#' @docType data
#' @family data
#' @examples
#' data(codes_disposal)
#' codes_disposal
"codes_disposal"

#' Cross-walk table for gear codes.
#'
#' Cross-walk table with gear codes and definitions.
#'
#' @format Tibble with rows for gear codes and columns for definitions.
#' @docType data
#' @family data
#' @examples
#' data(codes_gear)
#' codes_gear
"codes_gear"

#' Cross-walk table for group codes.
#'
#' Cross-walk table with group codes and definitions.
#'
#' @format Tibble with rows for group codes and columns for definitions.
#' @docType data
#' @family data
#' @examples
#' data(codes_group)
#' codes_group
"codes_group"

#' Cross-walk table for period codes.
#'
#' Cross-walk table with period codes and definitions.
#'
#' @format Tibble with rows for period codes and columns for definitions.
#' @docType data
#' @family data
#' @examples
#' data(codes_period)
#' codes_period
"codes_period"

#' Cross-walk table for source codes.
#'
#' Cross-walk table with source codes and definitions.
#'
#' @format Tibble with rows for source codes and columns for definitions.
#' @docType data
#' @family data
#' @examples
#' data(codes_source)
#' codes_source
"codes_source"

#' Conversion factors.
#'
#' Conversion factors: think metric.
#'
#' @format List with conversion factors.
#' @docType data
#' @family data
#' @examples
#' data(conv_factors)
#' conv_factors
"conv_factors"

#' Database information.
#'
#' Database information: connection, schemas, tables, and column names.
#'
#' @format List with database information: database connection and information
#' for tables: location, biosamples, catch, widths, surface spawn, macrocystis
#' spawn, understory spawn, and all spawn.
#' @docType data
#' @family data
#' @examples
#' data(database_info)
#' database_info
"database_info"

#' Unbalanced biosampling effort.
#'
#' Unbalanced sampling indicates the Section(s) and year(s) where unbalanced
#' biosampling occurred.
#' 1. In the Central Coast (CC) major SAR, Pacific Herring in Statistical Area
#' (SA) 08 tend to be smaller than fish in other areas. In addition, fewer
#' biological samples are typically collected from SA 08 compared to the other
#' areas. In 2014 and 2015, additional resources were available to collect
#' biological samples in SA 08, which consequently received more sampling effort
#' than previous years, compared to the other areas.
#'
#' @note This is currently not used in the `HerringData` package; users should
#'   consider weighting data from the indicated Section(s) and year(s) in their
#'   analyses to ensure sampling is representative.
#' @format List of lists with one element for each region indicating the
#'   Sections and year(s) that received unbalanced biosampling.
#' @docType data
#' @family data
#' @examples
#' data(unbalanced_sampling)
#' unbalanced_sampling
"unbalanced_sampling"

#' Undefined Sections.
#'
#' Spatially undefined Pacific Herring Sections, typically used for catch in a
#' known Statistical Area with an unknown Section. These Section codes end in
#' "0", for example, Section "050" indicates that catch is in Statistical Area
#' "05". Note that there are two exceptions that are actual areas: Sections
#' "220" and "280".
#'
#' @format Tibble with Statistical Areas and Sections.
#' @docType data
#' @family data
#' @examples
#' data(undefined_sections)
#' undefined_sections
"undefined_sections"
