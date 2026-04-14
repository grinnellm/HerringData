.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "This is HerringData version ", utils::packageVersion("HerringData"), "."
  )
}
