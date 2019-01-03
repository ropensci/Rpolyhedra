#' getPreloadedDataFilename
#'
#' Gets the path of Polyhedra preloaded data CSV file
#'
#' @param polyhedra_preloaded_data filename of polyhedra preloaded data csv
#' @return the path to the Polyhedra database file
#' @noRd
getPreloadedDataFilename <- function(polyhedra_preloaded_data =
                                       "polyhedra.preloaded.data.csv"){
  file.path(getDataDir(), polyhedra_preloaded_data)
}

#' updatePolyhedraDatabase
#'
#' Function for initializing database
#'
#' @param source.filenames if not null specify which source filenames to scrape
#' @return polyhedra db object
#' @noRd
updatePolyhedraDatabase <- function(source.filenames = NULL){
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

  setUserEnvir(".polyhedra", value = .polyhedra)

  .available.sources <- getPackageEnvir(".available.sources")
  .available.scrapping.conf <- getPackageEnvir(".available.scrapping.conf")


  #"dev-tetrahedron" "dev-minimal" "pkg-minimal" "fulldb"
  #Change when release version
  scrapePolyhedra(scrape.config = .available.scrapping.conf[["pkg-minimal"]],
                  source.filenames = source.filenames,
                  sources.config = .available.sources)
}

#' downloadRPolyhedraSupportingFiles
#'
#' Downloads the files from the remote location
#'
#' @return TRUE if sucessfull, FALSE otherwise
#' @importFrom utils unzip
#' @importFrom utils download.file
#' @importFrom utils zip
#' @noRd
downloadRPolyhedraSupportingFiles <- function(){
  retVal <- "SUCCESS"

  if (checkDatabaseVersion() == "UPDATE") {
    if (getDataEnv() == "HOME"){
      db.version <- getPackageDB()
      URL <- paste(
        "https://api.github.com/repos/qbotics/RpolyhedraDB/zipball/v",
        db.version,
        sep = "")
      td <- tempdir()
      zipFile <- tempfile(tmpdir = td, fileext = ".zip")
      #download file to tempfile
      oldw <- getOption("warn")
      options(warn = -1)
      retVal <-  tryCatch({
        utils::download.file(URL, destfile = zipFile, mode = "wb")
        "SUCCESS"
      }
      , error = function(e) {
        "NOT_AVAILABLE"
      })
      options(warn = oldw)
      if (retVal  == "SUCCESS") {
        utils::unzip(zipfile = zipFile, exdir = td)
        tmp.db.path <- list.files(path = td, pattern = "qbotics*")[1]
        files.to.copy <- list.files(file.path(td, tmp.db.path))
        #copy files
        file.copy(from = file.path(td, tmp.db.path, files.to.copy),
                  to = getUserSpace(), recursive = TRUE)
        #delete tmp path
        unlink(file.path(td, tmp.db.path), recursive = TRUE)
      }
    }
  }
  retVal
}

#' copyFilesToExtData
#'
#' Copies files from/to the home directory to the package one to build a preconfigured package.
#'
#' @param source.folder folder of polyhedra data sources
#' @param dest.folder   folder of polyhedra data destination
#' @param force indicate if existings directories must be overwritten
#' @return TRUE if sucessfull
#' @importFrom futile.logger flog.info
#' @noRd
copyFilesToExtData <- function(source.folder = getDataDir(data.env =  "HOME"),
                               dest.folder = getDataDir(data.env = "PACKAGE"),
                               force = FALSE){
  polyhedra.ledger <- getPolyhedraObject()$
    ledger$getAvailablePolyhedra(ret.fields = NULL)
  polyhedra.ledger.scraped <-
    polyhedra.ledger[polyhedra.ledger$status == "scraped", ]

  dir.create(dest.folder, showWarnings = FALSE, recursive = TRUE)
  #check existing sources
  existing <- FALSE
  .available.sources <- getPackageEnvir(".available.sources")
  for (source in names(.available.sources)){
    source.config <- .available.sources[[source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.exists(dest.folder.source)){
      existing <- TRUE
    }
  }
  if (existing & !force){
    stop(paste("Cannot copy files: they exists in destination",
               dest.folder.source, " Call ",
               "the function with force=TRUE or remove them manually"))
  }
  #clean dirs
  for (source in names(.available.sources)){
    source.config <- .available.sources[[source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.exists(dest.folder.source)){
      unlink(dest.folder.source, recursive = TRUE)
    }
    dir.create(dest.folder.source, showWarnings = FALSE, recursive = TRUE)
  }
  #copy files
  #copy version
  file.copy(file.path(source.folder, "version"), dest.folder,
            overwrite = TRUE)
  file.copy(file.path(source.folder, "polyhedra.preloaded.data.csv"),
            dest.folder, overwrite = TRUE)

  #copy polyhedra source files
  cont <- 0
  for (i in seq_len(nrow(polyhedra.ledger.scraped))){
    current.polyhedron <- polyhedra.ledger.scraped[i, ]
    source.config <- .available.sources[[current.polyhedron$source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.copy(file.path(source.config$getBaseDir(source.folder),
                            current.polyhedron$source.filename),
                  dest.folder.source)){
      cont <- cont + 1
    }
  }
  futile.logger::flog.info(paste("Copied", cont,
                                 "polyhedra sources files to",
                                 dest.folder))
  #copy RDS
  file.copy(file.path(source.folder, "polyhedra.RDS"), dest.folder)
  TRUE
}


#' PolyhedronScraperConfiguration
#'
#' Abstract class for configuring specific scrapers for Diferent sources
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{Initializes the object}
#' }
#' @importFrom     futile.logger flog.info
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfiguration.class <- R6::R6Class(
  "PolyhedronScraperConfiguration",
  public = list(
    name     = NA,
    base.dir = NA,
    initialize = function(name, base.dir) {
      self$name     <- name
      self$base.dir <- base.dir
      self
    },
    getName    = function(){
      self$name
    },
    getBaseDir = function(home.dir.data) {
      file.path(home.dir.data, "sources", self$base.dir)
    },
    getPolyhedraFiles = function(home.dir.data){
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    }))


#' PolyhedronScraperConfigurationNetlib
#'
#' Scraper configuration for Netlib source
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{getPolyhedraFiles(home.dir.data)}}{returns the file names on the netlib database}
#'   \item{\code{scrape(polyhedron.file.id, source.filename)}}{scrapes the object}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom futile.logger flog.info
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfigurationNetlib.class <- R6::R6Class(
  "PolyhedronScraperConfigurationNetlib",
  inherit = PolyhedronScraperConfiguration.class,
  public = list(
    initialize = function(name) {
      super$initialize(name = "netlib",
                       base.dir = file.path("www.netlib.org", "polyhedra"))
    },
    getPolyhedraFiles = function(home.dir.data){
      # wget -r -np -k http://www.netlib.org/polyhedra/ data/www.netlib.org
      polyhedra.dir   <- self$getBaseDir(home.dir.data)
      polyhedra.files <- dir(polyhedra.dir)
      polyhedra.files <- polyhedra.files[grep("[0-9]+", polyhedra.files)]
      polyhedra.files <- polyhedra.files[order(as.numeric(polyhedra.files))]
      polyhedra.files
    },
    scrape = function(polyhedron.file.id, source.filename){
      polyhedra.netlib.lines <- readLines(source.filename)
      polyhedron.file.id <- source.filename
      polyhedron.file.id <- strsplit(polyhedron.file.id, split = "/")[[1]]
      polyhedron.file.id <- polyhedron.file.id[length(polyhedron.file.id)]
      current.polyhedron <- Polyhedron.class$new(file.id = polyhedron.file.id)
      current.polyhedron$scrapeNetlib(netlib.p3.lines = polyhedra.netlib.lines)
      futile.logger::flog.debug(paste("parsed", source.filename, "with name",
                                      current.polyhedron$state$name))
      current.polyhedron
    }
  ))

#' PolyhedronScraperConfigurationDmccoey
#'
#' Scraper configuration for Dmccoey source
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{getPolyhedraFiles(home.dir.data)}}{returns the file names on the netlib database}
#'   \item{\code{scrape(polyhedron.file.id, source.filename)}}{scrapes the object}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom futile.logger flog.info
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfigurationDmccoey.class <- R6::R6Class(
  "PolyhedronScraperConfigurationDmccoey",
  inherit = PolyhedronScraperConfiguration.class,
  public = list(
    initialize = function(name) {
      super$initialize(name = "dmccooey",
                       base.dir = file.path("dmccooey.com", "polyhedra"))
      self
    },
    getPolyhedraFiles = function(home.dir.data){
      # wget -r -np -k http://dmccooey.com/polyhedra
      # for filename in `find dmccooey.com/polyhedra/*.html -type f`; do
      #   echo $filename
      #    wget "www."${filename::-5}".txt" -P dmccooey.com/polyhedra/
      # done

      polyhedra.dir   <- self$getBaseDir(home.dir.data)
      polyhedra.files <- dir(polyhedra.dir)
      polyhedra.files <- polyhedra.files[grep("\\.txt", polyhedra.files)]
      polyhedra.files
    },
    scrape = function(polyhedron.file.id, source.filename){
      polyhedra.dmccoey.lines <- readLines(source.filename)
      current.polyhedron <- Polyhedron.class$new(file.id = polyhedron.file.id)
      current.polyhedron$scrapeDmccoey(polyhedra.dmccoey.lines =
                                         polyhedra.dmccoey.lines)
      futile.logger::flog.debug(paste("parsed", source.filename, "with name",
                                      current.polyhedron$state$name))
      current.polyhedron
    }
  ))

#' checkDatabaseVersion
#'
#' Determines if there is a need for a database update by checking the
#' version file of both the package and the database installation.
#'
#' @return "UPDATE" if an update is required, "NO_ACTION_REQUIRED"
#'      otherwise.
#' @noRd
checkDatabaseVersion <- function(){
  status <- NULL
  database.version <- getDatabaseVersion()
  if (is.null(database.version)) {
    status <- "UPDATE"
  }
  package.version <- getPackageDB()
  if (!is.null(database.version)) {
    if (package.version > database.version) {
      status <- "UPDATE"
    } else {
      status <- "NO_ACTION_REQUIRED"
    }
  }
  status
}

#' isCompatiblePolyhedraRDS()
#'
#' Tests if the polyhedra RDS is compatible with the current format
#'
#' @param .polyhedra.candidate polyhedra db to test
#' @param halts indicates whether it has to halt execution when it
#' is not compatible
#' @importFrom futile.logger flog.info
#' @noRd
isCompatiblePolyhedraRDS <- function(.polyhedra.candidate =
                                       getPolyhedraObject(),
                                     halts = FALSE){
  file.class <- class(.polyhedra.candidate)
  compatible <- FALSE
  error <- ""

  if (file.class[[1]] == "PolyhedraDatabase"){
    db.version <- getPackageDB()
    compatible <- !is.null(db.version)
    if (compatible){
      compatible <- .polyhedra.candidate$getVersion() == db.version
    }
    else{
      db.version <- "#not-defined#"
    }

    if (!compatible){
      error <- paste("Incompatible! DB version observed = ",
                     .polyhedra.candidate$getVersion(),
                     " expected = ",
                     db.version,
                     ". Code version= ",
                     getPackageVersion(),
                     ".",
                     sep = "")
    }
  }
  else{
    error <- paste("Incompatible! PolyhedraDatabase class is ",
                   file.class[[1]],
                   ".",
                   sep = "")
  }
  if (nchar(error) > 0){
    if (halts){
      stop(paste(error, "Contact package mantainer."))
    }
    else{
      futile.logger::flog.error(error)
    }
  }
  compatible
}




#' getPackageVersion
#'
#' Obtains code version from the Description
#' @importFrom utils packageVersion
#' @noRd
getPackageVersion <- function(){
  as.character(utils::packageVersion("Rpolyhedra"))
}

#' getPackageDB
#'
#' Obtains the database version from environment
#' @noRd
getPackageDB <- function(){
  .package.db <- getPackageEnvir(".package.db")
  ret <- .package.db[[getPackageVersion()]]
  if (is.null(ret)){
    ret <- getPackageVersion()
  }
  ret
}

#' getDatabaseVersion
#'
#' Obtains the generation code version from the database version file
#' @noRd
getDatabaseVersion <- function(){
  version <- NULL
  version.file <- file.path(getDataDir(), "version")
  if (file.exists(version.file))
    version <- readLines(version.file, n = 1)
  version
}
