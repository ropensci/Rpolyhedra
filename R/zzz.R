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
    futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "data.io")
    futile.logger::flog.threshold(futile.logger::DEBUG)
    polyhedra.rds.file<-getPolyhedraRDSPath()
    if (file.exists(polyhedra.rds.file)) {
        polyhedra <- readRDS(getPolyhedraRDSPath())
    } else {
      polyhedra <- scrapePolyhedra(test = FALSE)
    }
    assign("polyhedra", value = polyhedra, envir = parent.env(environment()))
}
