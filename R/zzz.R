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

  #setup Available sources
  .available.sources <- list()
  .available.sources[["netlib"]] <- PolyhedronScraperConfigurationNetlib.class$new()
  .available.sources[["dmccooey"]] <- PolyhedronScraperConfigurationDmccoey.class$new()
  assign(".available.sources", value = .available.sources, envir = parent.env(environment()))
  .available.scrapping.conf <- list()
  .available.scrapping.conf[["dev-minimal"]] <- list(max.quant.config.schedule = 0,
                                                     max.quant.scrape = 10,
                                                     time2scrape.source = 20,
                                                      #20 seconds of building/scraping polyhedra database for reasonable devs timing
                                                     retry.scrape = FALSE)
  .available.scrapping.conf[["pkg-minimal"]] <- list(max.quant.config.schedule = 0,
                                                     max.quant.scrape = 0,
                                                     time2scrape.source = 80,
                                                     retry.scrape = FALSE)
  .available.scrapping.conf[["fulldb"]] <- list(max.quant.config.schedule = 0,
                                                     max.quant.scrape = 0,
                                                     time2scrape.source = 0,
                                                     retry.scrape = FALSE)
  assign(".available.scrapping.conf", value = .available.scrapping.conf, envir = parent.env(environment()))
  if (!exists(".data.env")){
    setDataDirEnvironment("PACKAGE")
  }
  if (!file.exists(getPreloadedDataFilename())){
    downloadRPolyhedraSupportingFiles()
  }
  .polyhedra <- NULL
  polyhedra.rds.file <- getPolyhedraRDSPath()
  if (file.exists(polyhedra.rds.file)) {
    polyhedra.candidate <- readRDS(polyhedra.rds.file)
    if (isCompatiblePolyhedraRDS(polyhedra.candidate, halts = TRUE)){
      .polyhedra <- polyhedra.candidate
    }
  }
  if (is.null(.polyhedra)){
    .polyhedra <- PolyhedraDatabase.class$new()
  }

  assign(".polyhedra", value = .polyhedra, envir = parent.env(environment()))
  scrapePolyhedra(.available.scrapping.conf[["pkg-minimal"]],
                  sources.config = .available.sources)
}

