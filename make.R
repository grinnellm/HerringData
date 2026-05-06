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
