.onAttach <- function(libname,
                      pkgname) {
  # Welcome message
  packageStartupMessage(
    "This is HerringData version ", utils::packageVersion("HerringData"), "."
  )
}
