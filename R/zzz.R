#' Executes code while loading the package.
#'
#' @param libname The library name
#' @param pkgname The package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (interactive() == FALSE) {
    rgl::open3d(useNULL = TRUE)
  }
  setPackageEnvir(variable.name = "RpolyhedraEnv", new.env(parent = asNamespace("Rpolyhedra")))

  # package version db
  .package.db <- list()
  # It is not necessary to put trivial compatibility
  .package.db[["0.2.5"]] <- "0.2.5"
  .package.db[["0.5.1"]] <- "0.5.0"

  setPackageEnvir(variable.name = ".package.db", .package.db)


  # setup Available sources
  .available.sources <- list()
  .available.sources[["netlib"]] <-
    PolyhedronScraperConfigurationNetlib$new()
  .available.sources[["dmccooey"]] <-
    PolyhedronScraperConfigurationDmccooey$new()

  setPackageEnvir(variable.name = ".available.sources", .available.sources)


  .available.scrapping.conf <- list()
  .available.scrapping.conf[["dev-tetrahedron"]] <-
    list(
      max.quant.config.schedule = 0,
      max.quant.scrape = 1,
      time2scrape.source = 2,
      # 2 seconds of building/scraping polyhedra database for
      # reasonable dev timing
      retry.scrape = FALSE
    )

  .available.scrapping.conf[["dev-minimal"]] <-
    list(
      max.quant.config.schedule = 0,
      max.quant.scrape = 10,
      time2scrape.source = 20,
      # 20 seconds of building/scraping polyhedra database for
      # reasonable dev timing
      retry.scrape = FALSE
    )
  .available.scrapping.conf[["pkg-minimal"]] <-
    list(
      max.quant.config.schedule = 0,
      max.quant.scrape = 0,
      time2scrape.source = 80,
      retry.scrape = FALSE
    )
  .available.scrapping.conf[["fulldb"]] <-
    list(
      max.quant.config.schedule = 0,
      max.quant.scrape = 0,
      time2scrape.source = 0,
      retry.scrape = FALSE
    )

  setPackageEnvir(
    variable.name = ".available.scrapping.conf",
    .available.scrapping.conf
  )

  initDataDirEnvironment()

  if (!file.exists(getPreloadedDataFilename())) {
    downloadRPolyhedraSupportingFiles()
  }

  updatePolyhedraDatabase()
}
