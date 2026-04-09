# Additional checks to run before `R-CMD-CHECK`

# Load packages
require(HerringData)

# Build the raw data files
source(file = here::here("data-raw", "pars.R"))

# Compile the supporting documents
devtools::build_manual(path = here::here("doc"))
devtools::build_vignettes(pkg = ".")

# Good practice (takes a while; restart R and require `HerringData` first)
goodpractice::gp(path = ".")
