#' Rpolyhedra
#'
#' polyhedra database
#'
#' A 142 polyhedra database scraped from PHD files <http://paulbourke.net/dataformats/phd/> as R6 objects and 'rgl' visualizing capabilities. The PHD format was created to describe the geometric polyhedra definitions derived mathematically <http://www.netlib.org/polyhedra/> by Andrew Hume and by the Kaleido program of Zvi Har'El.
#'
#' @docType package
#' @name Rpolyhedra
#' @import R6 futile.logger testthat
#' @author Alejandro Baranek <abaranek@dc.uba.ar>, Leonardo Javier Belen <leobelen@gmail.com>


#' Executes code while loading the package.
#'
#' @param libname Library name
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
    # Because of OSX warning on TZ
    Sys.setenv(TZ = "GMT")
    futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "data.io")
    futile.logger::flog.threshold(futile.logger::DEBUG)
    polyhedra.rds.file <- getPolyhedraRDSPath()
    if (file.exists(polyhedra.rds.file)) {
        polyhedra <- readRDS(getPolyhedraRDSPath())
    } else {
        polyhedra <- scrapePolyhedra(test = FALSE)
    }
    assign("polyhedra", value = polyhedra, envir = parent.env(environment()))
}
