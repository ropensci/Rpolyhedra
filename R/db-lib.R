#' getPackageEnvir
#'
#' @param variable.name  name of variable to be retrieved

#' Obtains a variable from package environment
getPackageEnvir <- function(variable.name){
  get(variable.name, envir = asNamespace("Rpolyhedra"))
}

#' setPackageEnvir
#'
#' Set a variable from package environment
#' @param variable.name  name of variable to be set
#' @param value          variable value

setPackageEnvir <- function(variable.name, value){
  assign(x = variable.name, value = value, envir = asNamespace("Rpolyhedra"))
}


#' getUserEnvir
#'
#' Gets a writable environment for the package
#'
#' @param variable.name  name of variable to be retrieved
getUserEnvir <- function(variable.name) {
  get(x = variable.name, envir = getPackageEnvir("RpolyhedraEnv"))
}

#' setUserEnvir
#'
#' Sets variable in the writable environment for the package
#'
#' @param variable.name  name of variable to be set
#' @param value          variable value

setUserEnvir <- function(variable.name, value) {
  assign(x = variable.name, value = value, envir =
           getPackageEnvir("RpolyhedraEnv"))
}


#' getDataEnv
#'
#' Gets the current .data.env value
#'
#' @return .data.env
getDataEnv <- function() {
  getUserEnvir(".data.env")
}


#' getUserSpace
#'
#' This function is used internally for accesing the local database path
#' @return path of user space
getUserSpace <- function(){
  file.path(path.expand("~"), ".R", "Rpolyhedra")
}

#' initDataDirEnvironment
#'
#' initialize data enviornment
#'
#' @return the data dir environment
initDataDirEnvironment <- function() {
  environment.filepath <- getEnvironmentFilepath()
  if (!file.exists(environment.filepath)){
    .data.env <- "PACKAGE"
  } else {
    .data.env <- readLines(environment.filepath)[1]
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}




#' getDataDir
#'
#' Gets the path of Rpolyhedra data dir.
#'
#' This function is used internally to determine whether the package
#' is compiled in source or package directory.
#' @param data.env enviroment where data directory must be returned
#' @return dir where the package access polyhedra database

getDataDir <- function(data.env = getDataEnv()) {
  data.dir <- ""
  if (data.env == "HOME") {
    data.dir <- getUserSpace()
  } else {
    data.dir <- getPackageDir()
  }
  data.dir
}

#' getEnvironmentFilepath
#'
#' Gets the filename where package data environment is persisted
#' @return The environment filepath

getEnvironmentFilepath <- function(){
  file.path(getDataDir("HOME"), "Rpolyhedra.env")
}

#' setDataDirEnvironment
#'
#' Sets the path of Rpolyhedra data dir.
#'
#' This function is used to set the data directories either to the package or the user home directory.
#'
#' @param env The type of environment to work with. Values are "PACKAGE" or "HOME" and it defaults to package
#' @return the curruent .data.env
setDataDirEnvironment <- function(env = "PACKAGE") {
  if (env %in% c("PACKAGE", "HOME")) {
    .data.env <- env
  }
  else {
    stop("Possible values are PACKAGE and HOME")
  }
  if (file.exists(getUserSpace())) {
    write(.data.env, getEnvironmentFilepath())
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}




#' getPackageDir
#'
#' Gets the path of package data.
getPackageDir <- function(){
  home.dir <- find.package("Rpolyhedra", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata/")
  if (!dir.exists(file.path(home.dir, data.subdir)))
    data.subdir <- "extdata/"
  file.path(home.dir, data.subdir)
}

#' getPolyhedraRDSPath
#'
#' Gets the path of Polyhedra RDS database file
#'
#' @param polyhedra_rds_filename filename of polyhedra database
#' @return the path to the Polyhedra database file
getPolyhedraRDSPath <- function(polyhedra_rds_filename = "polyhedra.RDS") {
  file.path(getDataDir(), polyhedra_rds_filename)
}

#' getPreloadedDataFilename
#'
#' Gets the path of Polyhedra preloaded data CSV file
#'
#' @param polyhedra_preloaded_data filename of polyhedra preloaded data csv
#' @return the path to the Polyhedra database file
getPreloadedDataFilename <- function(polyhedra_preloaded_data =
                                       "polyhedra.preloaded.data.csv"){
  file.path(getDataDir(), polyhedra_preloaded_data)
}

#' selectDataEnv
#'
#' Asks the user where to set the system variable .data.env
#'
#' @param env The environment to run on, can be PACKAGE, HOME or NULL. If null, it asks the user for a an Environment.
#' @usage
#'     selectDataEnv(env=NA)
#' @return .data.env
#' @importFrom futile.logger flog.info
selectDataEnv <- function(env=NA) {
  retVal <- "SUCCESS"
  if (is.na(env)) {
    if (!is.na(Sys.getenv(x = "ON_TRAVIS", unset = NA))) {
      return(TRUE)
    }
    accept.option <- readline(
      prompt = paste("Full Database needs to download data to home folder. ",
      "Agree [y/n]?:"))
    retry <- TRUE
    while (retry) {
      answer <- tolower(accept.option[1])
      if (answer == "n") {
        futile.logger::flog.info(paste("Working on demo DB. You can call",
                                 "selectDataEnv to use the full database."))
        setDataDirEnvironment("PACKAGE")
        retry <- FALSE
      } else if (answer == "y") {
        setDataDirEnvironment("HOME")
        retry <- FALSE
      }
      else {
        retry <- TRUE
      }
      if (retry) {
        accept.option <- readline(prompt = "Unknown option. Agree [y/n]?:")
      }
    }
  } else {
    setDataDirEnvironment(env)
  }
  .data.env <- getDataEnv()
  #loads the database
  if (.data.env == "HOME"){
    #create dir
    data.dir <- getUserSpace()
    if (!dir.exists(data.dir)) {
      dir.create(data.dir, recursive = TRUE, showWarnings = FALSE)
    }
    retVal <- downloadRPolyhedraSupportingFiles()
  }
  if(retVal == "SUCCESS") {
    updatePolyhedraDatabase()
  } else {
    setDataDirEnvironment("HOME")
  }


  retVal
}

#' updatePolyhedraDatabase
#'
#' Function for initializing database
#'
#' @param source.filenames if not null specify which source filenames to scrape
#'
#' @return polyhedra db object

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
  scrapePolyhedra(scrape.config = .available.scrapping.conf[["dev-minimal"]],
                  source.filenames = source.filenames,
                   sources.config = .available.sources)
}


#' getPolyhedraObject
#'
#' Gets the polyhedra object
#'
#' @return .polyhedra
getPolyhedraObject <- function() {
  getUserEnvir(".polyhedra")
}

#' downloadRPolyhedraSupportingFiles
#'
#' Downloads the files from the remote location
#'
#' @return TRUE if sucessfull, FALSE otherwise
#' @importFrom utils unzip
#' @importFrom utils download.file
#' @importFrom utils zip
#' @export
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
      }, error = function(e) {
        "NOT_AVAILABLE"
      })
      options(warn = oldw)
      if(retVal  == "SUCCESS")
      {
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
#' Copies files from the home directory to the package one to build a preconfigured package.
#'
#' @param force indicate if existings directories must be overwritten
#' @return TRUE if sucessfull
#' @importFrom futile.logger flog.info
copyFilesToExtData <- function(force = FALSE){
  polyhedra.ledger <- getPolyhedraObject()$
    ledger$getAvailablePolyhedra(ret.fields = NULL)
  polyhedra.ledger.scraped <-
    polyhedra.ledger[polyhedra.ledger$status == "scraped", ]
  data.env.home <- getDataDir(data.env = "HOME")
  data.env.package <- getDataDir(data.env = "PACKAGE")
  dir.create(data.env.package, showWarnings = FALSE, recursive = TRUE)
  #check existing sources
  existing <- FALSE
  .available.sources <- getPackageEnvir(".available.sources")
  for (source in names(.available.sources)){
    source.config <- .available.sources[[source]]
    dest.dir <- source.config$getBaseDir(data.env.package)
    if (file.exists(dest.dir)){
      existing <- TRUE
    }
  }
  if (existing & !force){
    stop(paste("Cannot copy files: they exists in destination. Call ",
                 "the function with force=TRUE or remove them manually"))
  }
  #clean dirs
  for (source in names(.available.sources)){
    source.config <- .available.sources[[source]]
    dest.dir <- source.config$getBaseDir(data.env.package)
    if (file.exists(dest.dir)){
      unlink(dest.dir, recursive = TRUE)
    }
    dir.create(dest.dir, showWarnings = FALSE, recursive = TRUE)
  }
  #copy files
  #copy version
  file.copy(file.path(data.env.home, "version"), data.env.package,
            overwrite = TRUE)
  file.copy(file.path(data.env.home, "polyhedra.preloaded.data.csv"),
            data.env.package, overwrite = TRUE)

  #copy polyhedra source files
  cont <- 0
  for (i in seq_len(nrow(polyhedra.ledger.scraped))){
    current.polyhedron <- polyhedra.ledger.scraped[i, ]
    source.config <- .available.sources[[current.polyhedron$source]]
    dest.dir <- source.config$getBaseDir(data.env.package)
    if (file.copy(file.path(source.config$getBaseDir(data.env.home),
                            current.polyhedron$filename),
                  dest.dir)){
        cont <- cont + 1
    }
  }
  futile.logger::flog.info(paste("Copied", cont,
                                 "polyhedra sources files to",
                                 data.env.package))
  #copy RDS
  file.copy(file.path(data.env.home, "polyhedra.RDS"), data.env.package)
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
PolyhedronScraperConfigurationNetlib.class <- R6::R6Class(
  "PolyhedronScraperConfigurationNetlib",
  inherit = PolyhedronScraperConfiguration.class,
  public = list(
    initialize = function(name) {
      super$initialize(name = "netlib", base.dir = "www.netlib.org/polyhedra/")
    },
    getPolyhedraFiles = function(home.dir.data){
      # wget -r -np -k http://www.netlib.org/polyhedra/ data/www.netlib.org/
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
PolyhedronScraperConfigurationDmccoey.class <- R6::R6Class(
  "PolyhedronScraperConfigurationDmccoey",
  inherit = PolyhedronScraperConfiguration.class,
  public = list(
    initialize = function(name) {
      super$initialize(name = "dmccooey", base.dir = "dmccooey.com/polyhedra/")
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

#' getGitCommit
#' get the last git commit sha
#' @param long.version determines if the complete version of the sha will
#'         be returned.
#' @importFrom git2r commits
#' @return String with git commit sha
#'
getGitCommit <- function(long.version = FALSE){
  #TODO: replace with git2r when issue #2 is resolved.
  #git2r::commits()[[1]]@sha
  if (file.exists(".git")){
    git.sha <- system("git log --pretty=format:'%h' -n 1", intern = TRUE)[1]
  }
  else{
    git.sha <- NA
  }
  if (long.version == FALSE) {
    git.sha <- substr(git.sha, 1, 7)
  }
  git.sha
}


#' PolyhedronTestTask
#'
#' Is an abstract class for specifying TestTask to make R6 iteration methods
#' like cover complaint with testhat infrastructure
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{run()}}{Run the test task}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom R6 R6Class
#'

PolyhedronTestTask.class <- R6::R6Class("PolyhedronTestTask",
  public = list(
    polyhedra.db = NA,
    source.config = NA,
    polyhedron.name = NA,
    initialize = function(polyhedra.db, source.config, polyhedron.name) {
      self$polyhedra.db    <- polyhedra.db
      self$source.config   <- source.config
      self$polyhedron.name <- polyhedron.name
      self
    },
    run = function(){
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    }))

#' PolyhedronTestTaskScrape
#'
#' A Test task for comparing a new scrape with an already scraped polyhedron
#' in the database
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{run()}}{Run the test task}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom futile.logger flog.info
#' @importFrom R6 R6Class
#'

PolyhedronTestTaskScrape.class <- R6::R6Class("PolyhedronTestTaskScrape.class",
  inherit = PolyhedronTestTask.class,
  public = list(
    polyhedra.dir = NA,
    polyhedron.file.id = NA,
    source.filename = NA,
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.file.id,
                          source.filename) {
      super$initialize(polyhedra.db = polyhedra.db,
                       source.config = source.config,
                       polyhedron.name = polyhedron.name)
      self$polyhedra.dir       <- polyhedra.dir
      self$polyhedron.file.id   <- polyhedron.file.id
      self$source.filename <- source.filename
      self
    },
    run = function(){
      source <- self$source.config$getName()
      tryCatch({
        obs    <- ""
        scraped.polyhedron <- self$source.config$scrape(polyhedron.file.id =
                      self$polyhedron.file.id,
                      file.path(self$polyhedra.dir, self$source.filename))
        scraped.name <- scraped.polyhedron$getName()
        status <- "testing"
      },
      error = function(e){
        error <- paste(e$message, collapse = ",")
        futile.logger::flog.error(paste("catched error", error))
        assign("error", error, envir = parent.env(environment()))
        status <- "exception"
        if (exists("scraped.polyhedron")){
          obs    <- scraped.polyhedron$getErrors()
        }
      })
      expected.polyhedron <-
                self$polyhedra.db$getPolyhedron(source = source,
                                                polyhedron.name = scraped.name)

      expected.polyhedron$getState()$expect_equal(scraped.polyhedron)
    }))

#' PolyhedronTestTaskEdgesConsistency
#'
#' A Test task for running edges consistency test for current polyhedron

PolyhedronTestTaskEdgesConsistency.class <- R6::R6Class(
  "PolyhedronTestTaskEdgesConsistency",
  inherit = PolyhedronTestTask.class,
  public = list(
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.file.id, source.filename) {
      super$initialize(polyhedra.db = polyhedra.db, source.config =
                         source.config,
                       polyhedron.name = polyhedron.name)
      self
    },
    run = function(){
      source <- self$source.config$getName()
      current.polyhedron <-
        self$polyhedra.db$getPolyhedron(source = source,
                                        polyhedron.name = self$polyhedron.name)
      edges.inconsistent <- current.polyhedron$state$checkEdgesConsistency()
      expect_equal(nrow(edges.inconsistent), 0)
    }))

#' getPackageVersion
#'
#' Obtains code version from the Description
#' @importFrom utils packageVersion
getPackageVersion <- function(){
  as.character(utils::packageVersion("Rpolyhedra"))
}

#' getPackageDB
#'
#' Obtains the database version from environment
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
getDatabaseVersion <- function(){
  version <- NULL
  version.file <- file.path(getDataDir(), "version")
  if (file.exists(version.file))
    version <- readLines(version.file, n = 1)
  version
}

#' checkDatabaseVersion
#'
#' Determines if there is a need for a database update by checking the
#' version file of both the package and the database installation.
#'
#' @return "UPDATE" if an update is required, "NO_ACTION_REQUIRED"
#'      otherwise.
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


#' PolyhedraDatabase
#'
#' Scrapes all polyhedra in data folder to save a representation which
#' is accesible by the final users upon call to \code{getPolyhedron()}.
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{Initializes the object}
#'   \item{\code{existsSource(source)}}{Determines if the source exists on
#'   the database}
#'   \item{\code{getPolyhedraSourceDir(source)}}{Retrieves polyhedra dir
#'   of a source}
#'   \item{\code{addSource(source)}}{Adds a new source to the database}
#'   \item{\code{configPolyhedraRDSPath()}}{config path for rds database file}
#'   \item{\code{existsPolyhedron(source,polyhedron.name)}}{Determines if
#'   the polyhedron exists on the database}
#'   \item{\code{getPolyhedron(source, polyhedron.name, strict)}}{Retrieves
#'   a polyhedron by source and name}
#'   \item{\code{addPolyhedron(source,polyhedron,overwrite)}}{Adds a polyhedron
#'   by source and name, if orverwrite is TRUE, it will update any existing one
#'   by that source and name}
#'   \item{\code{configPolyhedraSource(source.config, source.filenames, max.quant)}}{Scrapes all
#'   polyhedra in the given directory for adding to db or testing}
#'   \item{\code{schedulePolyhedraSources(sources.config, source.filenames, max.quant,
#'   test)}}{Scrapes files applying parameter sources.config}
#'   \item{\code{getAvailablePolyhedra(sources,search.string)}}{Retrieves
#'   all polyhedron within the source those names match with search.string}
#' }
#'
#' @field polyhedra.rds.file path of rds database file
#' @field sources.config Sources configuration for scraping different sources
#' @field ledger rr ledger of scraping process
#' @field data Polyhedra data from different sources
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom futile.logger flog.info
#' @importFrom utils zip
#' @importFrom R6 R6Class
PolyhedraDatabase.class <- R6::R6Class("PolyhedraDatabase",
  public = list(
    version = NA,
    polyhedra.rds.file = NA,
    sources.config = NA,
    ledger         = NA,
    initialize = function() {
      self$version        <- getDatabaseVersion()
      self$ledger         <- ScraperLedger.class$new()
      self$sources.config <- list()
      self
    },
    getVersion = function(){
      self$version
    },
    configPolyhedraRDSPath = function(){
      self$polyhedra.rds.file <- getPolyhedraRDSPath()
      self$polyhedra.rds.file
    },
    existsSource = function(source){
      source %in% self$getAvailableSources()
    },
    addSourceConfig = function(source.config) {
      source <- source.config$getName()
      if (!self$existsSource(source)){
        self$sources.config[[source]] <- source.config
        self$getPolyhedraSourceDir(source)
      }
      self
    },
    existsPolyhedron = function(source = "netlib", polyhedron.name) {
      ret <- FALSE
      file.path <- self$getPolyhedronFilename(source = source,
                                        polyhedron.name = polyhedron.name,
                                        extension = ".RDS.zip")
      ret <- !is.null(file.path)
      if (ret) {
        ret <- file.exists(file.path)
      }
      ret
    },
    getPolyhedraSourceDir = function(source, create.dir = TRUE){
      ret <- file.path(getDataDir(), "polyhedra", source, "/")
      if (create.dir){
        dir.create(ret, showWarnings = FALSE, recursive = TRUE)
      }
      ret
    },
    getPolyhedronFilename = function(source, polyhedron.name, extension){
      paste(self$getPolyhedraSourceDir(source),
            self$ledger$getCRCPolyhedronName(source = source,
                polyhedron.name = polyhedron.name),
            extension, sep = "")
    },
    getPolyhedron = function(source = "netlib", polyhedron.name,
                             strict = FALSE) {
      data.dir <- self$getPolyhedraSourceDir(source = source)
      if (!self$existsPolyhedron(source = source, polyhedron.name =
                                 polyhedron.name)){
        message <- paste("Polyhedron", polyhedron.name,
                         "not available in source", source)
        if (strict) {
          stop(message)
        }
      }
      ret <- NULL
      crc.name <- self$ledger$getCRCPolyhedronName(source = source,
                    polyhedron.name = polyhedron.name)
      zip.filename <- file.path(data.dir, paste(crc.name, ".RDS.zip", sep = ""))
      serialized.polyhedron <- NULL
      if (file.exists(zip.filename)) {
        tmp.dir <- file.path(tempdir = tempdir(), source)
        dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
        serialized.filename <- paste(crc.name, ".RDS", sep = "")
        tmp.filename <- file.path(tmp.dir, serialized.filename)
        unzip(zipfile = zip.filename, files = serialized.filename,
              exdir = tmp.dir)
        serialized.polyhedron <- readRDS(file = tmp.filename)
        unlink(tmp.filename)
      }
      if (!is.null(serialized.polyhedron)){
        ret <- Polyhedron.class$new(file.id = NA)
        ret$deserialize(serialized.polyhedron = serialized.polyhedron)
      }
      ret
    },
    addPolyhedron = function(source="netlib", source.filename,
                             polyhedron, overwrite = FALSE,
                             pretend = TRUE) {
      polyhedron.name <- polyhedron$getName()
      data.dir <- self$getPolyhedraSourceDir(source = source)
      prev.data <- self$getPolyhedron(source = source,
                                      polyhedron.name = polyhedron.name)
      if (!overwrite & !is.null(prev.data) & !pretend){
        futile.logger::flog.info(paste("Polyhedron",
                                       polyhedron.name,
                                       "in source",
                                       source,
                                       "already in database"))
      }
      else {
        if (!pretend){
          crc.name <- self$ledger$getCRCPolyhedronName(source = source,
                                                       polyhedron.name = polyhedron.name)
          serialized.polyhedron <- polyhedron$state$serialize()
          tmp.dir <- file.path(tempdir = tempdir(), source)
          dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
          serialized.filename <- paste(crc.name, ".RDS", sep = "")
          tmp.filename <- file.path(tmp.dir, serialized.filename)
          saveRDS(object = serialized.polyhedron, ascii = TRUE,
                  file = tmp.filename)
          zip(zipfile = file.path(data.dir, paste(crc.name,
                                                  ".RDS.zip",
                                                  sep = "")),
              files = tmp.filename, flags = "-j")
          unlink(tmp.filename)
          futile.logger::flog.info(paste("Added polyhedron in file",
                                         polyhedron.name,
                                         "#|n",
                                         polyhedron$file.id,
                                         polyhedron.name,
                                         "in source",
                                         source,
                                         "to database with CRC",
                                         crc.name))
        }
      }
      self$ledger$updateStatus(source = source,
                               source.filename = source.filename,
                               status = "scraped",
                               scraped.polyhedron = polyhedron)
      polyhedron
    },
    configPolyhedraSource = function(source.config, source.filenames= NULL,
                                     max.quant = 0,
                                     pretend = TRUE) {
      source <- source.config$getName()
      home.dir.data <- getDataDir()
      self$configPolyhedraRDSPath()
      futile.logger::flog.debug(paste("configuring source", source))
      polyhedra.dir   <- source.config$getBaseDir(home.dir.data)
      polyhedra.files <- source.config$getPolyhedraFiles(home.dir.data)
      if (!is.null(source.filenames)){
        polyhedra.files <- polyhedra.files[
                      polyhedra.files %in% source.filenames]
      }
      if (length(polyhedra.files) > 0) {
        if (max.quant > 0){
          polyhedra.files <- polyhedra.files[1:min(max.quant,
                                                   length(polyhedra.files))]
        }
        self$addSourceConfig(source.config)
        scheduled <- NULL
        for (source.filename in polyhedra.files) {
          if (is.null(self$ledger$getIdFilename(source = source,
                                source.filename = source.filename))){
            self$ledger$addFilename(source = source,
                                    source.filename = source.filename)
            scheduled <- c(scheduled, source.filename)
          }
        }
        if (length(scheduled) > 0){
          futile.logger::flog.info(paste("Scheduling source ",
                                         source,
                                         length(scheduled),
                                         "filenames:",
                                         paste(scheduled, collapse = ",")))
        }

      }
      if (!pretend){
        self$saveRDS()
      }
      self
    },
    saveRDS = function(){
      ret <- NULL
      if (self$ledger$dirty){
        self$ledger$updateCalculatedFields()
        futile.logger::flog.info(paste("Saving RDS in file",
                                       self$polyhedra.rds.file))
        ret <- saveRDS(self, self$polyhedra.rds.file)
      }
      ret
    },
    cover = function(mode,
                     sources = names(self$sources.config),
                     covering.code,
                     polyhedra.names = NULL,
                     max.quant=0,
                     pretend = TRUE,
                     seed = NULL){
      self$configPolyhedraRDSPath()
      if (!is.null(seed)){
        set.seed(seed)
        max.quant.retrieve <- 0
      }
      else{
        max.quant.retrieve <- max.quant
      }
      filenames2scrape <- self$ledger$getFilenamesStatusMode(mode = mode,
                                        sources = sources,
                                        max.quant = max.quant.retrieve,
                                        order.by.vertices.faces = TRUE)
      if (!is.null(polyhedra.names)){
        filenames2scrape  <- filenames2scrape[filenames2scrape$scraped.name
                                              %in% polyhedra.names, ]
      }

      ret <- list()
      home.dir.data <- getDataDir()
      if (!is.null(filenames2scrape)){
        if (!is.null(seed)){
          n <- nrow(filenames2scrape)
          if (max.quant < n){
            sample.2.cover <- sort(sample(1:n, size = max.quant))
          }
          else{
            sample.2.cover <- 1:n
          }
          filenames2scrape <- filenames2scrape[sample.2.cover, ]
        }
        for (r in seq_len(nrow(filenames2scrape))){
          current.filename.data <- filenames2scrape[r, ]
          source <- current.filename.data$source
          source.config <- self$sources.config[[source]]
          polyhedra.dir <- source.config$getBaseDir(home.dir.data)
          polyhedron.file.id <- current.filename.data$file.id
          source.filename <- current.filename.data$source.filename
          ret[[paste(source, source.filename, sep = "|")]] <-
                  covering.code(polyhedra.dir = polyhedra.dir,
                                source.config = source.config,
                                polyhedron.file.id = polyhedron.file.id,
                                source.filename = source.filename)
        }
        #after covering, save RDS
        if (!pretend){
          self$saveRDS()
        }
      }
      ret
    },
    scrape = function(mode = "scrape.queued",
                      sources = names(self$sources.config),
                      max.quant = 0,
                      time2scrape.source = 30,
                      pretend = TRUE){
      scrape.function <- function(polyhedra.dir, source.config,
                                  polyhedron.file.id, source.filename){
        source <- source.config$getName()
        current.polyhedron <- NULL
        tryCatch({
          self$ledger$updateStatus(source = source,
                                   source.filename = source.filename,
                                   status = "scraping")
          current.polyhedron <- source.config$scrape(
            polyhedron.file.id = polyhedron.file.id,
            file.path(polyhedra.dir,
                      source.filename))
          if (current.polyhedron$isChecked()){
            self$addPolyhedron(source = source,
                               source.filename = source.filename,
                               polyhedron = current.polyhedron)
          }
          else{
            errors <- current.polyhedron$getErrors()
            self$ledger$updateStatus(source = source,
                                     source.filename = source.filename,
                                     status = "failed", obs = errors)
          }
        },
        error = function(e){
          error <- paste(e$message, collapse = ",")
          futile.logger::flog.error(paste("catched error", error))
          assign("error", error, envir = parent.env(environment()))
          self$ledger$updateStatus(source = source,
                                   source.filename,
                                   status = "exception",
                                   obs = error)
        })
        current.polyhedron
      }
      if (time2scrape.source > 0){
        max.quant.time <- self$ledger$getSizeToTimeScrape(sources = sources,
                            time2scrape = time2scrape.source)
      }
      else{
        max.quant.time <- 0
      }
      if (max.quant > 0 & max.quant.time > 0){
        max.quant.scrape <- min(max.quant, max.quant.time)
      }
      else{
       #if 1 parameters is 0, then max for not executing 0
        max.quant.scrape <- max(max.quant,
                                max.quant.time)
      }

      futile.logger::flog.debug(paste("Scraping sources",
                                      paste(sources, collapse = ","),
                                        "max.quant =", max.quant,
                                        "time2scrape.source =",
                                        time2scrape.source,
                                        "max.quant.time =",
                                        max.quant.time,
                                        "max.quant.scrape (min) =",
                                        max.quant.scrape))
      self$cover(mode          = mode,
                 sources       = sources,
                 covering.code = scrape.function,
                 max.quant     = max.quant.scrape,
                 pretend       = pretend)
    },
    testRR = function(sources = names(self$sources.config),
                    max.quant = 0){
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!isCompatiblePolyhedraRDS(polyhedra.db.saved)){
          stop("Incompatible polyhedra.db saved. Contact package mantainer.")
        }
      }else{
        # if database doesn't exists setup test as false
        futile.logger::ERROR(paste("There is no polyhedra database so ",
                               "test could not be runned"))
        test <- FALSE
      }
      test.function <- function(polyhedra.dir,
                                source.config,
                                polyhedron.file.id,
                                source.filename){
        source <- source.config$getName()
        scraped.polyhedron <- NULL
        tryCatch({
          scraped.polyhedron <- source.config$scrape(
            polyhedron.file.id = polyhedron.file.id,
            source.filename = file.path(polyhedra.dir, source.filename))
          polyhedron.name <- scraped.polyhedron$getName()
          status <- "testing"
          obs    <- ""
        },
        error = function(e){
          error <- paste(e$message, collapse = ",")
          futile.logger::flog.error(paste("catched error", error))
          assign("error", error, envir = parent.env(environment()))
          status <- "exception"
          obs    <- scraped.polyhedron$getErrors()
        })
        self$ledger$updateStatus(source = source,
                                 source.filename = source.filename,
                                 status.field = "status.test",
                                 status = status,
                                 obs = obs)
        if (status == "testing"){
          tryCatch({
            res <- expect_true(self$existsPolyhedron(source = source,
                      polyhedron.name = polyhedron.name))
            db.polyhedron <- self$getPolyhedron(source = source,
                      polyhedron.name = polyhedron.name)
            scraped.polyhedron$state$inferEdges()
            expect_equal(scraped.polyhedron, db.polyhedron)
            status <- "tested"
          },
          error = function(e){
            error <- paste(e$message, collapse = ",")
            futile.logger::flog.error(paste("catched error", error))
            assign("error", error, envir = parent.env(environment()))
            status <- "failed"
            obs    <- scraped.polyhedron$getErrors()
          })
          self$ledger$updateStatus(source = source,
                                   source.filename = source.filename,
                                   status.field = "status.test",
                                   status = status, obs = obs)
        }
        scraped.polyhedron
      }
      ret <- self$cover(mode          = "test",
                        sources       = sources,
                        covering.code = test.function,
                        max.quant     = max.quant)
      ret
    },
    generateTestTasks = function(sources = names(self$sources.config),
                                 polyhedra.names = NULL,
                                 TestTaskClass,
                                 max.quant = 0){
      seed <- getPackageVersion()
      seed <- as.numeric(gsub("v|\\.", "", seed))
      seed <- seed * 121
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!isCompatiblePolyhedraRDS(polyhedra.db.saved)){
          stop("Incompatible polyhedra.db saved. Contact package mantainer.")
        }
      }else{
        # if database doesn't exists setup test as false
        futile.logger::ERROR(paste("There is no polyhedra database so test ",
                                   "could not be runned"))
        test <- FALSE
      }
      test.task.gen.function <- function(polyhedra.dir, source.config,
                                         polyhedron.file.id, source.filename){
        scraped.polyhedron <- NULL
        source <- source.config$getName()
        polyhedron.ledger <- polyhedra.db.saved$ledger$
          df[polyhedra.db.saved$ledger$getIdFilename(source, source.filename), ]
        polyhedron.name   <- polyhedron.ledger$scraped.name
        #debug
        polyhedron.ledger <<- polyhedron.ledger

        task <- TestTaskClass$new(polyhedra.db = polyhedra.db.saved,
                                  source.config = source.config,
                                  polyhedron.name = polyhedron.name,
                                  polyhedra.dir = polyhedra.dir,
                                  polyhedron.file.id = polyhedron.file.id,
                                  source.filename = source.filename)
        task
      }
      ret <- self$cover(mode            = "test",
                        sources         = sources,
                        covering.code   = test.task.gen.function,
                        polyhedra.names = polyhedra.names,
                        max.quant       = max.quant,
                        seed          = seed)
      ret
    },
    schedulePolyhedraSources = function (sources.config =
                                         getPackageEnvir(".available.sources"),
                                         source.filenames= NULL,
                                       max.quant = 0,
                                       test = FALSE,
                                       pretend = TRUE){
      for (source  in names(sources.config)){
        self$configPolyhedraSource(source.config = sources.config[[source]],
                                   source.filenames = source.filenames,
                                   max.quant = max.quant,
                                   pretend = pretend)
      }
      self
    },
    getAvailableSources = function() {
      #TODO in ledger
      self$ledger$getAvailableSources()
    },
    getAvailablePolyhedra = function(sources = self$getAvailableSources(),
                                     search.string = NULL, ignore.case = TRUE) {
      self$ledger$getAvailablePolyhedra(sources = sources,
                                        search.string = search.string,
                                        ignore.case = ignore.case)
    }
  ))

#' isCompatiblePolyhedraRDS()
#'
#' Tests if the polyhedra RDS is compatible with the current format
#' @param .polyhedra.candidate polyhedra db to test
#' @param halts indicates whether it has to halt execution when it
#' is not compatible
#'
#' @importFrom futile.logger flog.info
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


#' switchToFullDatabase()
#'
#' Prompts user for changing database to fulldb in
#' user filespace
#'
#' @param env The environment to run on, can be PACKAGE,
#' HOME or NA. If NA, it asks the user for a an Environment.
#' @usage
#'     switchToFullDatabase(env=NA)
#' @return .data.env
#' @export

switchToFullDatabase <- function(env = NA){
  retVal <- selectDataEnv(env = env)
  if(retVal == "NOT_AVAILABLE")
  {
    futile.logger::flog.error("Full Database not available yet.");
  }
}



#' scrapePolyhedra()
#'
#' Gets polyhedra objects from text files of
#' different sources, scheduling and scraping using
#' predefined configurations
#'
#' @param scrape.config predefined configuration for scraping
#' @param source.filenames if not null specify which source filenames to scrape
#' @param sources.config the sources that will be used by the function
#' @return polyhedra db object
scrapePolyhedra <- function(scrape.config,
                            source.filenames = NULL,
                sources.config = getPackageEnvir(".available.sources")){
  scrapePolyhedraSources(max.quant.config.schedule =
                     scrape.config[["max.quant.config.schedule"]],
                   max.quant.scrape = scrape.config[["max.quant.scrape"]],
                   time2scrape.source = scrape.config[["time2scrape.source"]],
                   sources.config = sources.config,
                   source.filenames = source.filenames,
                   retry.scrape = scrape.config[["retry.scrape"]])

}

#' scrapePolyhedraSources()
#'
#' Method for obtaining polyhedra objects from text files of
#' different sources, scheduling and scraping
#'
#' @param sources.config the sources that will be used by the function
#' @param max.quant.config.schedule number of files to schedule
#' @param max.quant.scrape number of files scrape
#' @param time2scrape.source time applied to scrape source
#' @param source.filenames if not null specify which source filenames to scrape
#' @param retry.scrape should it retry scrape?
#' @return polyhedra db object
#' @usage
#'     scrapePolyhedraSources(sources.config =
#'          getPackageEnvir(".available.sources"),
#'     max.quant.config.schedule = 0,
#'     max.quant.scrape = 0, time2scrape.source = 30,
#'     source.filenames = NULL, retry.scrape = FALSE)
scrapePolyhedraSources <- function(sources.config =
                                    getPackageEnvir(".available.sources"),
                                  max.quant.config.schedule = 0,
                                  max.quant.scrape = 0,
                                  time2scrape.source = 30,
                                  source.filenames = NULL,
                                  retry.scrape = FALSE){
  futile.logger::flog.debug(paste("Scheduling",
                                  max.quant.config.schedule,
                                  "polyhedra for scraping"))
  getPolyhedraObject()$schedulePolyhedraSources(sources.config =
                                                  sources.config,
                                                source.filenames =
                                                  source.filenames,
                                      max.quant = max.quant.config.schedule,
                                      pretend = FALSE)
  if (retry.scrape){
    mode <- "scrape.retry"
  }
  else {
    mode <- "scrape.queued"
  }
  futile.logger::flog.debug(paste("Scraping",
                                  max.quant.scrape,
                                  "polyhedra up to",
                                  time2scrape.source, "seconds"))
  getPolyhedraObject()$scrape(mode = mode,
                              max.quant = max.quant.scrape,
                              time2scrape.source = time2scrape.source,
                              pretend = FALSE)
  #All files not scraped in building, marked as skipped
  still.queued <- which(getPolyhedraObject()$ledger$df$status == "queued")
  if (length(still.queued) > 0){
    apply(getPolyhedraObject()$ledger$df[still.queued, ], MARGIN = 1,
          FUN = function(x) {
            getPolyhedraObject()$ledger$
              updateStatus(source = x["source"],
                           source.filename = x["source.filename"],
                           status = "skipped",
                           obs = "#TODO in next release")
            }
          )
    #save skipped state in RDS file
    getPolyhedraObject()$saveRDS()
  }
  getPolyhedraObject()
}


#' getAvailableSources()
#'
#' Gets the list of names of available sources in database
#'
#' @return sources string vector
#' @export
#' @examples
#' #gets all sources in the database
#'available.sources <- getAvailableSources()
#'
#'#returns all polyhedra from all sources
#'available.polyhedra <- getAvailablePolyhedra(sources=available.sources)
#'
#'#search within the polyhedron names from all sources
#'cubes <- getAvailablePolyhedra(sources=available.sources,
#'         search.string="cube")
getAvailableSources <- function(){
  getPolyhedraObject()$getAvailableSources()
}

#' getAvailablePolyhedra()
#'
#' Gets the list of names of available polyhedra and its status in
#' the polyhedra database, which can be later called with getPolyhedron
#'
#' @importFrom futile.logger flog.info
#' @param sources A source of polyhedra. Available sources are netlib, dmccooey
#' @param search.string A search string
#' @return polyhedra names vector
#' @export
#' @usage
#'     getAvailablePolyhedra(sources, search.string)
#' @examples
#'
#' #gets all polyhedra in the database
#'available.polyhedra <- getAvailablePolyhedra()
#'
#'#returns all polyhedra from a given source, in this case, netlib
#'available.netlib.polyhedra <- getAvailablePolyhedra(sources="netlib")
#'
#'#search within the polyhedron names
#'
#'cube <- getAvailablePolyhedra(sources="netlib",search.string="cube")

getAvailablePolyhedra <- function(sources =
                          names(getPackageEnvir(".available.sources")),
                        search.string = NULL){
  getPolyhedraObject()$getAvailablePolyhedra(sources = sources,
                                             search.string = search.string)
}


#' getPercentilPolyhedraQuant
#' returns polyhedra quantity of parameter percentil
#' @param percentil is the percentil which must be applied
#' to estimate the figure
#' @param quant.min minimum quantity of files to return
#'
getPercentilPolyhedraQuant <- function(percentil, quant.min = 100){
  max(round(percentil * nrow(getAvailablePolyhedra())), quant.min)
}
#' getPolyhedron()
#'
#' Gets a polyhedron from the database. It returns an R6 Class
#' with all its characteristics and functions.
#' The object returned, of type Polyhedron.class, allows to the
#' user to get access to all the functionality provided.
#'
#' @param source source name
#' @param polyhedron.name  a valid name of a polyhedron in
#' the database. Current names can be found with getAvailablePolyhedra()
#' @importFrom futile.logger flog.info
#' @export
#' @return polyhedron R6 object
#' @export
#' @examples
#' tetrahedron <- getPolyhedron(source="netlib",
#'        polyhedron.name = 'tetrahedron')
#'
#' # returns name of polyhedra
#' tetrahedron$getName()
#'
#' # polyhedron state
#' tetrahedron.state <- tetrahedron$getState()
#'
#' # Johnson symbol and Schlafli symbol
#' tetrahedron.state$getSymbol()
#'
#' # vertex data.frame
#' tetrahedron.state$getVertices()
#'
#' # List of faces of solid representation (3D)
#' tetrahedron.state$getSolid()
#'
#' # List of faces of net representation (2D)
#' tetrahedron.state$getNet()

getPolyhedron <- function(source = "netlib", polyhedron.name) {
  ret <- NULL
  if (exists(".polyhedra", envir = getPackageEnvir("RpolyhedraEnv"))) {
    ret <- getPolyhedraObject()$getPolyhedron(source = source,
                                        polyhedron.name = polyhedron.name)
  }
  ret
}
