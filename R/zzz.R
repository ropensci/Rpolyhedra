#' Rpolyhedra
#'
#' polyhedra database
#'
#' A polyhedra database scraped from:
#' * http://paulbourke.net/dataformats/phd/: PHD files as R6 objects and 'rgl'
#'          visualizing capabilities. The PHD format was created to describe the geometric
#'          polyhedra definitions derived mathematically <http://www.netlib.org/polyhedra/>
#'          by Andrew Hume and by the Kaleido program of Zvi Har'El.
#' * http://dmccooey.com/Polyhedra: Polyhedra text datafiles.
#'
#' @docType package
#' @name Rpolyhedra
#' @import R6 futile.logger testthat
#' @author Alejandro Baranek <abaranek@dc.uba.ar>, Leonardo Javier Belen <leobelen@gmail.com>

#' Executes code while loading the package.
#'
#' @param libname The library name
#' @param pkgname The package name
.onLoad <- function(libname, pkgname) {
  #debug settings to run the logger.
  #futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "data.io")
  #futile.logger::flog.threshold(futile.logger::INFO)
  polyhedra.rds.file <- getPolyhedraRDSPath()

  #setup Available sources
  .available.sources <- list()
  .available.sources[["netlib"]] <- PolyhedronScraperConfigurationNetlib.class$new()
  .available.sources[["dmccooey"]] <- PolyhedronScraperConfigurationDmccoey.class$new()
  assign(".available.sources", value = .available.sources, envir = parent.env(environment()))
  if (file.exists(polyhedra.rds.file)) {
    polyhedra.candidate <- readRDS(polyhedra.rds.file)
    if (compatiblePolyhedraRDS(polyhedra.candidate)){
      .polyhedra <- polyhedra.candidate
    }
    else{
      stop("Incompatible polyhedra database found. Reinstall")
    }
  }
  else{
    .polyhedra <- PolyhedronDatabase.class$new()
  }
  assign(".polyhedra", value = .polyhedra, envir = parent.env(environment()))
  scrapePolyhedraSources(max.quant.config.schedule = 50,
                         max.quant.scrape = 8,
                         time2scrape.source = 10, #80 seconds of building scraping polyhedra
                         sources.config = .available.sources,
                         retry.scrape = FALSE)
}
