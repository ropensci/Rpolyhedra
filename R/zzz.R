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
#' @importFrom R6 R6Class
#' @importFrom futile.logger flog.info
#' @author Alejandro Baranek <abaranek@dc.uba.ar>, Leonardo Javier Belen <leobelen@gmail.com>

#' Executes code while loading the package.
#'
#' @param libname The library name
#' @param pkgname The package name
.onLoad <- function(libname, pkgname) {

  setPackageEnvir(variable.name = "RpolyhedraEnv", new.env())

  #package version db
  .package.db <- list()
  #It is not necesary to put trivial compatibility
  .package.db[["0.2.5"]] <- "0.2.5"

  setPackageEnvir(variable.name = ".package.db", .package.db)


  #setup Available sources
  .available.sources <- list()
  .available.sources[["netlib"]] <-
    PolyhedronScraperConfigurationNetlib.class$new()
  .available.sources[["dmccooey"]] <-
    PolyhedronScraperConfigurationDmccoey.class$new()

  setPackageEnvir(variable.name = ".available.sources", .available.sources)


  .available.scrapping.conf <- list()
  .available.scrapping.conf[["dev-tetrahedron"]] <-
    list(max.quant.config.schedule = 0,
       max.quant.scrape = 1,
       time2scrape.source = 2,
       #2 seconds of building/scraping polyhedra database for
       #reasonable dev timing
       retry.scrape = FALSE)

  .available.scrapping.conf[["dev-minimal"]] <-
    list(max.quant.config.schedule = 0,
       max.quant.scrape = 10,
       time2scrape.source = 20,
       #20 seconds of building/scraping polyhedra database for
       #reasonable dev timing
       retry.scrape = FALSE)
  .available.scrapping.conf[["pkg-minimal"]] <-
    list(max.quant.config.schedule = 0,
       max.quant.scrape = 0,
       time2scrape.source = 80,
       retry.scrape = FALSE)
  .available.scrapping.conf[["fulldb"]] <-
    list(max.quant.config.schedule = 0,
       max.quant.scrape = 0,
       time2scrape.source = 0,
       retry.scrape = FALSE)

  setPackageEnvir(variable.name = ".available.scrapping.conf",
                  .available.scrapping.conf)

  initDataDirEnvironment()

  if (!file.exists(getPreloadedDataFilename())){
    downloadRPolyhedraSupportingFiles()
  }

  updatePolyhedraDatabase()

}
