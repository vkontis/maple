.onLoad <- function(libname, pkgname) {
    packageStartupMessage("Loading required package: INLA")
    library(INLA)
}