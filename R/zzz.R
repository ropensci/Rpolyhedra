#' rpolyhedra
#'
#' A package to replace the now defunct poly command.
#'
#' @docType package
#' @name rpolyhedra
#' @import R6 futile.logger testthat
#' @author Alejandro Baranek <alejandrobaranek@gmail.com>, Leonardo Javier Belen <leobelen@gmail.com>

#' Executes code while loading the package.
#'
#' @param libname Library name
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
    # Because of OSX warning on TZ
    Sys.setenv(TZ = "GMT")
    dir.create("log", showWarnings = FALSE, recursive = TRUE)
    polyhedra.rds.file<-getPolyhedraRDSPath()
    if (file.exists(polyhedra.rds.file)) {
        polyhedra <- readRDS(getPolyhedraRDSPath())
    } else {
      #TODO remove
      #polyhedra <- scrapePolyhedra(max.quant = 15, polyhedra.rds.file = polyhedra.rds.filename,test = FALSE)
      polyhedra <- scrapePolyhedra(test = FALSE)
    }
    assign("polyhedra", value = polyhedra, envir = .GlobalEnv)
}
