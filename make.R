# Additional checks to run before `R-CMD-CHECK`

# Load packages
require(HerringData)

# Build the raw data files
source(file = here::here("data-raw", "pars.R"))

# Compile the supporting documents
devtools::build_manual(path = here::here("docs"))
pkgdown::build_articles()

# Good practice (takes a while; restart R and require `HerringData` first)
require(HerringData)
goodpractice::gp(path = ".")

# TODO:
# 1. Make vignette for data summary reports.
# 2. Make and publish the HTML files (vignettes) on the GitHub site so that the
#    README file can point to them.
