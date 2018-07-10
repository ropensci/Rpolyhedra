#' getPackageEnvir
#'
#' @param variable.name  name of variable to be retrieved

#' Obtains a variable from package environment
getPackageEnvir <- function(variable.name){
  get(variable.name,envir=asNamespace("Rpolyhedra"))
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
  get(x=variable.name , envir = getPackageEnvir("RpolyhedraEnv"))
}

#' setUserEnvir
#'
#' Sets variable in the writable environment for the package
#'
#' @param variable.name  name of variable to be set
#' @param value          variable value

setUserEnvir <- function(variable.name, value) {
  assign(x=variable.name , value = value, envir = getPackageEnvir("RpolyhedraEnv"))
}


#' getDataEnv
#'
#' Gets the current .data.env value
#'
#' @return .data.env
getDataEnv <- function() {
  #get(".data.env", envir = getUserEnv())
  getUserEnvir(".data.env")
  #dev-package-env
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
  if(!file.exists(environment.filepath)){
    .data.env <- "PACKAGE"
  }else{
    .data.env <- readLines(environment.filepath)[1]
  }

  setUserEnvir(".data.env", value = .data.env)
  #assign(".data.env", value = .data.env, envir = getUserEnv())
  #dev-package-env

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

getDataDir <- function(data.env=getDataEnv()) {
  data.dir <- ""
  if(data.env == "HOME")
  {
    data.dir <- getUserSpace()
  }
  else{
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
setDataDirEnvironment <- function(env="PACKAGE") {
  if(env %in% c("PACKAGE","HOME")){
    .data.env <- env
  }
  else{
    stop("Possible values are PACKAGE and HOME")
  }
  if (file.exists(getUserSpace())){
    write(.data.env, getEnvironmentFilepath())
  }

  setUserEnvir(".data.env", value = .data.env)
  #assign(".data.env", value = .data.env, envir = getUserEnv())
  #dev-package-env

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
getPreloadedDataFilename <- function(polyhedra_preloaded_data = "polyhedra.preloaded.data.csv"){
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
#' @import futile.logger
selectDataEnv <- function(env=NA) {
  if(is.na(env)) {
    if(!is.na(Sys.getenv(x = "ON_TRAVIS", unset=NA)))
      return(TRUE)
    accept.option <- readline(prompt="Full Database needs to download data to home folder. Agree [y/n]?:")
    retry <- TRUE
    while(retry) {
      answer <- tolower(accept.option[1])
      if(answer== "n") {
        futile.logger::flog.info("Working on demo DB. You can call selectDataEnv to use the full database.")
        setDataDirEnvironment("PACKAGE")
        retry <- FALSE
      } else if(answer == "y") {
        setDataDirEnvironment("HOME")
        retry <- FALSE
      }
      else{
        retry <- TRUE
      }
      if (retry){
        accept.option <- readline(prompt="Unknown option. Agree [y/n]?:")
      }
    }
  }
  #loads the database
  .data.env <-getDataEnv()
  if (.data.env=="HOME"){
    #create dir
    data.dir <- getUserSpace()
    if(!dir.exists(data.dir)) {
      dir.create(data.dir, recursive=TRUE, showWarnings = FALSE)
    }
    downloadRPolyhedraSupportingFiles()
  }
  updatePolyhedraDatabase()
  .data.env
}

#' updatePolyhedraDatabase
#'
#' Function for initializing database
#'
#'
#' @return polyhedra db object

updatePolyhedraDatabase <- function(){
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
  #assign(".polyhedra", value = .polyhedra, envir = getUserEnv())
  #dev-package-env

  .available.sources <- getPackageEnvir(".available.sources")
  .available.scrapping.conf <- getPackageEnvir(".available.scrapping.conf")

  scrapePolyhedra(scrape.config = .available.scrapping.conf[["pkg-minimal"]],
                  sources.config = .available.sources)
}


#' getPolyhedraObject
#'
#' Gets the polyhedra object
#'
#' @return .polyhedra
getPolyhedraObject <- function() {
  getUserEnvir(".polyhedra")
  #get(".polyhedra", envir = getUserEnv())
  #dev-package-env
}

#' downloadRPolyhedraSupportingFiles
#'
#' Downloads the files from the remote location
#'
#' @return TRUE if sucessfull, FALSE otherwise
#' @import utils
#' @export
downloadRPolyhedraSupportingFiles <- function(){
  if(checkDatabaseVersion() == "UPDATE")
  {
    if(getDataEnv() == "HOME"){
      db.version <- getPackageDB()
      URL <- paste("https://api.github.com/repos/qbotics/RpolyhedraDB/zipball/v", db.version, sep="")
      td <- tempdir()
      zipFile <- tempfile(tmpdir=td, fileext=".zip")
      #download file to tempfile
      download.file(URL, destfile = zipFile, mode="wb")
      utils::unzip(zipfile = zipFile, exdir = td)
      tmp.db.path <- list.files(path = td, pattern="qbotics*")[1]
      files.to.copy <- list.files(file.path(td, tmp.db.path))
      #copy files
      file.copy(from = file.path(td,tmp.db.path, files.to.copy), to=getUserSpace(), recursive = TRUE)
      #delete tmp path
      unlink(file.path(td,tmp.db.path), recursive=TRUE)
      return(TRUE)
    }
  }
  return(TRUE)
}

#' copyFilesToExtData
#'
#' Copies files from the home directory to the package one to build a preconfigured package.
#'
#' @param force indicate if existings directories must be overwritten
#' @return TRUE if sucessfull
#' @import utils
#' @import futile.logger
copyFilesToExtData <- function(force = FALSE){
  polyhedra.ledger <- getPolyhedraObject()$ledger$getAvailablePolyhedra(ret.fields = NULL)
  polyhedra.ledger.scraped <- polyhedra.ledger[polyhedra.ledger$status=="scraped",]
  data.env.home <- getDataDir(data.env ="HOME")
  data.env.package <- getDataDir(data.env ="PACKAGE")
  dir.create(data.env.package,showWarnings = FALSE, recursive = TRUE)
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
    stop(paste("Cannot copy files: they exists in destination. Call the function with force=TRUE or remove them manually"))
  }
  #clean dirs
  for (source in names(.available.sources)){
    source.config <- .available.sources[[source]]
    dest.dir <- source.config$getBaseDir(data.env.package)
    if (file.exists(dest.dir)){
      unlink(dest.dir,recursive = TRUE)
    }
    dir.create(dest.dir, showWarnings = FALSE, recursive = TRUE)
  }
  #copy files
  #copy version
  file.copy(file.path(data.env.home,"version"),data.env.package,overwrite = TRUE)
  file.copy(file.path(data.env.home,"polyhedra.preloaded.data.csv"),data.env.package,overwrite = TRUE)

  #copy polyhedra source files
  cont <- 0
  for (i in c(1:nrow(polyhedra.ledger.scraped))){
    current.polyhedron <-polyhedra.ledger.scraped[i,]
    source.config <- .available.sources[[current.polyhedron$source]]
    dest.dir <- source.config$getBaseDir(data.env.package)
    if (file.copy(file.path(source.config$getBaseDir(data.env.home),current.polyhedron$filename),
                  dest.dir)){
        cont <- cont+1
    }
  }
  futile.logger::flog.info(paste("Copied",cont,"polyhedra sources files to",data.env.package))
  #copy RDS
  file.copy(file.path(data.env.home,"polyhedra.RDS"),data.env.package)
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
#' @import     futile.logger
#' @importFrom R6 R6Class
PolyhedronScraperConfiguration.class <- R6::R6Class("PolyhedronScraperConfiguration",
  public = list(
    name     = NA,
    base.dir = NA,
    initialize = function(name,base.dir) {
      self$name     <- name
      self$base.dir <- base.dir
      self
    },
    getName    = function(){
      self$name
    },
    getBaseDir = function(home.dir.data) {
      file.path(home.dir.data, self$base.dir)
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
#'   \item{\code{scrape(polyhedron.number, polyhedron.filename)}}{scrapes the object}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import futile.logger
#' @importFrom R6 R6Class
PolyhedronScraperConfigurationNetlib.class <- R6::R6Class("PolyhedronScraperConfigurationNetlib",
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
    scrape = function(polyhedron.number, polyhedron.filename){
      polyhedra.netlib.lines <- readLines(polyhedron.filename)
      polyhedron.number <- polyhedron.filename
      polyhedron.number <- strsplit(polyhedron.number,split = "/")[[1]]
      polyhedron.number <- as.numeric(polyhedron.number[length(polyhedron.number)])
      current.polyhedron <- Polyhedron.class$new(number=polyhedron.number)
      current.polyhedron$scrapeNetlib(netlib.p3.lines = polyhedra.netlib.lines)
      futile.logger::flog.debug(paste("parsed", polyhedron.filename, "with name",
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
#'   \item{\code{scrape(polyhedron.number, polyhedron.filename)}}{scrapes the object}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import stringr futile.logger
#' @importFrom R6 R6Class
PolyhedronScraperConfigurationDmccoey.class <- R6::R6Class("PolyhedronScraperConfigurationDmccoey",
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
    scrape = function(polyhedron.number, polyhedron.filename){
      polyhedra.dmccoey.lines <- readLines(polyhedron.filename)
      current.polyhedron <- Polyhedron.class$new(number=polyhedron.number)
      current.polyhedron$scrapeDmccoey(polyhedra.dmccoey.lines = polyhedra.dmccoey.lines)
      futile.logger::flog.debug(paste("parsed", polyhedron.filename, "with name",
                                      current.polyhedron$state$name))
      current.polyhedron
    }
  ))

#' getGitCommit
#' get the last git commit sha
## @import git2r
#' @return String with git commit sha
#'
getGitCommit <- function(){
  #TODO: replace with git2r when issue #2 is resolved.
  #git2r::commits()[[1]]@sha
  if (file.exists(".git")){
    git_id<-system("git log --pretty=format:'%h' -n 1",intern=TRUE)[1]
  }
  else{
    git_id<-NA
  }
  git_id
}


#' PolyhedronTestTask
#'
#' Is an abstract class for specifying TestTask to make R6 iteration methods like cover
#' complaint with testhat infrastructure
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
#' A Test task for comparing a new scrape with an already scraped polyhedron in database
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
#' @import futile.logger
#' @importFrom R6 R6Class
#'

PolyhedronTestTaskScrape.class <- R6::R6Class("PolyhedronTestTaskScrape.class",
  inherit = PolyhedronTestTask.class,
  public = list(
    polyhedra.dir = NA,
    polyhedron.number = NA,
    polyhedron.filename = NA,
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.number,
                          polyhedron.filename) {
      super$initialize(polyhedra.db = polyhedra.db, source.config = source.config,
                       polyhedron.name = polyhedron.name)
      self$polyhedra.dir       <- polyhedra.dir
      self$polyhedron.number   <- polyhedron.number
      self$polyhedron.filename <- polyhedron.filename
      self
    },
    run = function(){
      source <- self$source.config$getName()
      tryCatch({
        obs    <- ""
        scraped.polyhedron <- self$source.config$scrape(polyhedron.number = self$polyhedron.number,
                                                   file.path(self$polyhedra.dir, self$polyhedron.filename))
        scraped.name <- scraped.polyhedron$getName()
        scraped.polyhedron$getRGLModel(1, c(0, 0, 0))
        futile.logger::flog.debug(paste("generated RGLModel"))
        status <- "testing"
      },
      error=function(e){
        error <- paste(e$message,collapse=",")
        futile.logger::flog.error(paste("catched error",error))
        assign("error", error, envir = parent.env(environment()))
        status <- "exception"
        if (exists("scraped.polyhedron")){
          obs    <- scraped.polyhedron$getErrors()
        }
      })
      expected.polyhedron <-
                self$polyhedra.db$getPolyhedron(source = source,
                                                polyhedron.name = scraped.name)

      #debug gp
      #print(scraped.polyhedron)
      #print(expected.polyhedron)
      #expected.polyhedron <<- expected.polyhedron
      #task <<- self

      expect_equal(scraped.polyhedron, expected.polyhedron)

    }))

#' PolyhedronTestTaskEdgesConsistency
#'
#' A Test task for running edges consistency test for current polyhedron

PolyhedronTestTaskEdgesConsistency.class <- R6::R6Class("PolyhedronTestTaskEdgesConsistency",
  inherit = PolyhedronTestTask.class,
  public = list(
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.number, polyhedron.filename) {
      super$initialize(polyhedra.db = polyhedra.db, source.config = source.config,
                       polyhedron.name = polyhedron.name)
      self
    },
    run = function(){
      source <- self$source.config$getName()
      current.polyhedron <-
        self$polyhedra.db$getPolyhedron(source = source,
                                        polyhedron.name = self$polyhedron.name)
      edges.inconsistent <- current.polyhedron$state$checkEdgesConsistency()
      expect_equal(nrow(edges.inconsistent),0)
    }))

#' getPackageVersion
#'
#' Obtains code version from the Description
getPackageVersion <- function(){
  as.character(packageVersion("Rpolyhedra"))
}

#' getPackageDB
#'
#' Obtains the database version from environment
getPackageDB <- function(){
  .package.db <-getPackageEnvir(".package.db")
  .package.db[[getPackageVersion()]]
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
#' Determines if there is a need for a database update by checking the version file of both
#' the package and the database installation.
#'
#' @return "UPDATE" if an update is required, "NO_ACTION_REQUIRED" otherwise.
checkDatabaseVersion <- function(){
  status <- NULL
  database.version <- getDatabaseVersion()
  if(is.null(database.version)) {
    status <- "UPDATE"
  }
  package.version <- getPackageDB()

  if(!is.null(database.version))
  {
    if(package.version > database.version) {
      status <- "UPDATE"
    } else {
      status <- "NO_ACTION_REQUIRED"
    }
  }
  status
}


#' PolyhedraDatabase
#'
#' Scrapes all polyhedra in data folder to save a representation which is accesible by the final users upon call to \code{getPolyhedron()}.
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{Initializes the object}
#'   \item{\code{existsSource(source)}}{Determines if the source exists on the database}
#'   \item{\code{getSource(source, strict=False)}}{Retrieves a source by name}
#'   \item{\code{addSource(source)}}{Adds a new source to the database}
#'   \item{\code{configPolyhedraRDSPath()}}{config path for rds database file}
#'   \item{\code{existsPolyhedron(source,polyhedron.name)}}{Determines if the polyhedron exists on the database}
#'   \item{\code{getPolyhedron(source, polyhedron.name, strict)}}{Retrieves a polyhedron by source and name}
#'   \item{\code{addPolyhedron(source,polyhedron,overwrite)}}{Adds a polyhedron by source and name, if orverwrite is TRUE,
#'           it will update any existing one by that source and name}
#'   \item{\code{configPolyhedraSource(source.config, max.quant)}}{Scrapes all polyhedra in the given
#'           directory for adding to db or testing}
#'   \item{\code{schedulePolyhedraSources(sources.config,max.quant, test)}}{Scrapes files applying parameter sources.config}
#'   \item{\code{getAvailablePolyhedra(sources,search.string)}}{Retrieves all polyhedron within the source those names
#'           match with search.string}
#' }
#'
#' @field polyhedra.rds.file path of rds database file
#' @field sources.config Sources configuration for scraping different sources
#' @field ledger rr ledger of scraping process
#' @field data Polyhedra data from different sources
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import futile.logger
#' @importFrom R6 R6Class
PolyhedraDatabase.class <- R6::R6Class("PolyhedraDatabase",
  public = list(
    version = NA,
    polyhedra.rds.file = NA,
    sources.config = NA,
    ledger         = NA,
    data           = NA,
    initialize = function() {
      self$version        <- getDatabaseVersion()
      self$ledger         <- ScraperLedger.class$new()
      self$sources.config <- list()
      self$data           <- list()
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
      ret <- FALSE
      if (length(self$data)>0){
        ret <- source %in% names(self$data)
      }
      ret
    },
    getSource = function(source, strict = FALSE) {
      ret <- NULL
      if (strict & !self$existsSource(source)){
        stop(paste("Source",source,"not available in polyhedra database"))
      }
      self$data[[source]]
    },
    addSourceConfig = function(source.config) {
      source <- source.config$getName()
      if (!self$existsSource(source)){
        self$sources.config[[source]] <- source.config
        self$data[[source]] <- list()
      }
      self
    },
    existsPolyhedron = function(source = "netlib",polyhedron.name) {
      ret <- FALSE
      source.data <- self$getSource(source)
      if (length(source.data)>0){
        ret <- polyhedron.name %in% names(source.data)
      }
      ret
    },
    getPolyhedron = function(source = "netlib",polyhedron.name, strict = FALSE) {
      source.data <- self$getSource(source)
      if (!self$existsPolyhedron(source,polyhedron.name)){
        message <- paste("Polyhedron",polyhedron.name,"not available in source",source)
        if (strict){
          stop(message)
        }
      }
      source.data[[polyhedron.name]]
    },
    addPolyhedron = function(source="netlib",polyhedron.filename,
                             polyhedron,overwrite=FALSE) {
      polyhedron.name <- polyhedron$getName()
      prev.data <- self$getPolyhedron(source = source, polyhedron.name = polyhedron.name)
      if (!overwrite &!is.null(prev.data)){
        futile.logger::flog.info(paste("Polyhedron", polyhedron.name,"in source",source,"already in database"))
      }
      else {
        self$data[[source]][[polyhedron.name]]<-polyhedron
        futile.logger::flog.info(paste("Added polyhedron in file",polyhedron.name,"#|n", polyhedron$number, polyhedron.name,"in source",source,"to database"))
      }
      self$ledger$updateStatus(source = source,filename = polyhedron.filename,
                               status = "scraped",scraped.polyhedron = polyhedron)
      polyhedron
    },
    configPolyhedraSource = function(source.config, max.quant = 0) {
      source <- source.config$getName()
      home.dir.data <- getDataDir()
      self$configPolyhedraRDSPath()
      futile.logger::flog.debug(paste("configuring source", source))
      polyhedra.dir   <- source.config$getBaseDir(home.dir.data)
      polyhedra.files <- source.config$getPolyhedraFiles(home.dir.data)
      if (length(polyhedra.files)>0) {
        if (max.quant >0){
          polyhedra.files <- polyhedra.files[1:min(max.quant,length(polyhedra.files))]
        }
        self$addSourceConfig(source.config)
        scheduled <- NULL
        for (polyhedron.filename in polyhedra.files) {
          if (is.null(self$ledger$getIdFilename(source = source,filename = polyhedron.filename))){
            self$ledger$addFilename(source = source, filename = polyhedron.filename)
            scheduled <- c(scheduled, polyhedron.filename)
          }
        }
        if (length(scheduled)>0){
          futile.logger::flog.info(paste("Scheduling source ",source, length(scheduled), "filenames:",
                                         paste(scheduled, collapse = ",")))
        }

      }
      self$saveRDS()
      self
    },
    saveRDS = function(){
      ret <- NULL
      if (self$ledger$dirty){
        self$ledger$updateCalculatedFields()
        futile.logger::flog.info(paste("Saving RDS in file",self$polyhedra.rds.file))
        ret <- saveRDS(self, self$polyhedra.rds.file)
      }
      ret
    },
    cover = function(mode,
                     sources = names(self$sources.config),
                     covering.code,
                     max.quant=0,
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
      ret <- list()
      home.dir.data <- getDataDir()
      if (!is.null(filenames2scrape)){
        if (!is.null(seed)){
          sample.2.cover <- sort(sample(1:nrow(filenames2scrape),size = max.quant))
          filenames2scrape <- filenames2scrape[sample.2.cover,]
        }
        n <- nrow(filenames2scrape)
        for (r in c(1:n)){
          current.filename.data <- filenames2scrape[r,]
          source <- current.filename.data$source
          source.config <- self$sources.config[[source]]
          polyhedra.dir <- source.config$getBaseDir(home.dir.data)
          polyhedron.number <- current.filename.data$number
          polyhedron.filename <- current.filename.data$filename
          ret[[paste(source,polyhedron.filename,sep="|")]]<-
                  covering.code(polyhedra.dir, source.config,
                                polyhedron.number, polyhedron.filename)
        }
        #after covering, save RDS
        self$saveRDS()
      }
      ret
    },
    scrape = function(mode = "scrape.queued",
                      sources = names(self$sources.config),
                      max.quant = 0,
                      time2scrape.source = 30){
      scrape.function <- function(polyhedra.dir, source.config, polyhedron.number, polyhedron.filename){
        source <- source.config$getName()
        current.polyhedron <- NULL
        tryCatch({
          self$ledger$updateStatus(source = source,filename = polyhedron.filename,
                                 status = "scraping")
          current.polyhedron <- source.config$scrape(polyhedron.number = polyhedron.number, file.path(polyhedra.dir, polyhedron.filename))
          if (current.polyhedron$isChecked()){
            current.polyhedron$getRGLModel(1, c(0, 0, 0))
            futile.logger::flog.debug(paste("generated RGLModel"))
            self$addPolyhedron(source=source, polyhedron.filename = polyhedron.filename,
                               polyhedron = current.polyhedron)
          }
          else{
            errors <- current.polyhedron$getErrors()
            self$ledger$updateStatus(source,polyhedron.filename,status = "failed",obs=errors)
          }
        },
        error=function(e){
          error <- paste(e$message,collapse=",")
          futile.logger::flog.error(paste("catched error",error))
          assign("error", error, envir = parent.env(environment()))
          self$ledger$updateStatus(source,polyhedron.filename,status = "exception",obs=error)
        })
        current.polyhedron
      }
      if (time2scrape.source >0){
        max.quant.time <- self$ledger$getSizeToTimeScrape(sources = sources, time2scrape = time2scrape.source)
      }
      else{
        max.quant.time <- 0
      }
      if (max.quant >0 & max.quant.time >0){
        max.quant.scrape <- min(max.quant, max.quant.time)
      }
      else{
       #if 1 parameters is 0, then max for not executing 0
        max.quant.scrape <- max(max.quant, max.quant.time)
      }

      futile.logger::flog.debug(paste("Scraping sources",paste(sources,collapse=","),
                                "max.quant =",max.quant,
                                "time2scrape.source =",time2scrape.source,
                                "max.quant.time =",max.quant.time,
                                "max.quant.scrape (min) =",max.quant.scrape))
      self$cover(mode          = mode,
                 sources       = sources,
                 covering.code = scrape.function,
                 max.quant     = max.quant.scrape)
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
        futile.logger::ERROR("There is no polyhedra database so test could not be runned")
        test <- FALSE
      }
      test.function <- function(polyhedra.dir, source.config, polyhedron.number, polyhedron.filename){
        source <- source.config$getName()
        scraped.polyhedron <- NULL
        tryCatch({
          scraped.polyhedron <- source.config$scrape(polyhedron.number = polyhedron.number, file.path(polyhedra.dir, polyhedron.filename))
          polyhedron.name <- scraped.polyhedron$getName()
          status <- "testing"
          obs    <- ""
        },
        error=function(e){
          error <- paste(e$message,collapse=",")
          futile.logger::flog.error(paste("catched error",error))
          assign("error", error, envir = parent.env(environment()))
          status <- "exception"
          obs    <- scraped.polyhedron$getErrors()
        })
        self$ledger$updateStatus(source,polyhedron.filename, status.field = "status.test", status = status, obs= obs)
        if (status=="testing"){
          tryCatch({
            res <- expect_true(self$existsPolyhedron(source = source,
                                              polyhedron.name = polyhedron.name))
            db.polyhedron <- self$getPolyhedron(source = source,
                                                polyhedron.name = polyhedron.name)
            #debug
            #scraped.polyhedron <<- scraped.polyhedron
            #db.polyhedron <<- db.polyhedron
            scraped.polyhedron$state$inferEdges()
            expect_equal(scraped.polyhedron,db.polyhedron)
            status <- "tested"
          },
          error=function(e){
            error <- paste(e$message,collapse=",")
            futile.logger::flog.error(paste("catched error",error))
            assign("error", error, envir = parent.env(environment()))
            status <- "failed"
            obs    <- scraped.polyhedron$getErrors()
          })
          self$ledger$updateStatus(source,polyhedron.filename, status.field = "status.test", status = status, obs= obs)
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
                                 TestTaskClass,
                                 max.quant = 0){
      seed <- getPackageVersion()
      seed <- as.numeric(gsub("v|\\.","",seed))
      seed <- seed *121
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!isCompatiblePolyhedraRDS(polyhedra.db.saved)){
          stop("Incompatible polyhedra.db saved. Contact package mantainer.")
        }
      }else{
        # if database doesn't exists setup test as false
        futile.logger::ERROR("There is no polyhedra database so test could not be runned")
        test <- FALSE
      }
      test.task.gen.function <- function(polyhedra.dir, source.config, polyhedron.number, polyhedron.filename){
        scraped.polyhedron <- NULL
        source <- source.config$getName()
        polyhedron.ledger <- polyhedra.db.saved$ledger$df[polyhedra.db.saved$ledger$getIdFilename(source,polyhedron.filename),]
        polyhedron.name   <- polyhedron.ledger$scraped.name

        task <- TestTaskClass$new(polyhedra.db = polyhedra.db.saved,
                                  source.config = source.config, polyhedron.name = polyhedron.name,
                                  polyhedra.dir = polyhedra.dir, polyhedron.number = polyhedron.number,
                                  polyhedron.filename = polyhedron.filename)
        task
      }
      ret <- self$cover(mode          = "test",
                        sources       = sources,
                        covering.code = test.task.gen.function,
                        max.quant     = max.quant,
                        seed          = seed)
      ret
    },
    schedulePolyhedraSources=function (sources.config = getPackageEnvir(".available.sources"), max.quant = 0, test = FALSE){
      for (source  in names(sources.config)){
        self$configPolyhedraSource(source.config = sources.config[[source]],
                                   max.quant = max.quant)
      }
    },
    getAvailableSources = function() {
      names(self$data)
    },
    getAvailablePolyhedra = function(sources = self$getAvailableSources(),
                                     search.string = NULL,ignore.case=TRUE) {
      self$ledger$getAvailablePolyhedra(sources = sources,
                                        search.string = search.string,
                                        ignore.case = ignore.case)
    }
  ))

#' isCompatiblePolyhedraRDS()
#'
#' Tests if the polyhedra RDS is compatible with the current format
#' @param .polyhedra.candidate polyhedra db to test
#' @param halts indicates whether it has to halt execution when it is not compatible
#'
#' @import futile.logger
isCompatiblePolyhedraRDS <- function(.polyhedra.candidate = getPolyhedraObject(), halts = FALSE){
  file.class <- class(.polyhedra.candidate)
  compatible <- FALSE
  error <- ""

  if (file.class[[1]]=="PolyhedraDatabase"){
    db.version <- .package.db[[getPackageVersion()]]
    compatible <- .polyhedra.candidate$getVersion()==db.version
    if (!compatible){
      error <- paste("Incompatible! DB version observed= ",.polyhedra.candidate$getVersion(),
                     " expected ",db.version, ". Code version= ",getPackageVersion(), ".", sep="")
    }
  }
  else{
    error <- paste("Incompatible! PolyhedraDatabase class is ",file.class[[1]],".", sep ="")
  }
  if (nchar(error)>0){
    if (halts){
      stop(paste(error,"Contact package mantainer."))
    }
    else{
      futile.logger::flog.error(error)
    }
  }
  compatible
}


#' switchToFullDatabase()
#'
#' Prompts user for changing database to fulldb in user filespace
#'
#' @param env The environment to run on, can be PACKAGE, HOME or NA. If NA, it asks the user for a an Environment.
#' @usage
#'     switchToFullDatabase(env=NA)
#' @return .data.env
#' @export

switchToFullDatabase <- function(env=NA){
  selectDataEnv(env=env)
}



#' scrapePolyhedra()
#'
#' Gets polyhedra objects from text files of
#' different sources, scheduling and scraping using predefined configurations
#'
#' @param scrape.config predefined configuration for scraping
#' @param sources.config the sources that will be used by the function
#' @return polyhedra db object
scrapePolyhedra <- function(scrape.config,
                sources.config = getPackageEnvir(".available.sources")){
  scrapePolyhedraSources(max.quant.config.schedule = scrape.config[["max.quant.config.schedule"]],
                         max.quant.scrape = scrape.config[["max.quant.scrape"]],
                         time2scrape.source = scrape.config[["time2scrape.source"]],
                         sources.config = sources.config,
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
#' @param retry.scrape should it retry scrape?
#' @return polyhedra db object
#' @usage
#'     scrapePolyhedraSources(sources.config = getPackageEnvir(".available.sources"),
#'     max.quant.config.schedule = 0,
#'     max.quant.scrape = 0, time2scrape.source = 30, retry.scrape = FALSE)
scrapePolyhedraSources<- function(sources.config = getPackageEnvir(".available.sources"),
                                  max.quant.config.schedule = 0,
                                  max.quant.scrape = 0,
                                  time2scrape.source = 30,
                                  retry.scrape = FALSE){
  futile.logger::flog.debug(paste("Scheduling",max.quant.config.schedule, "polyhedra for scraping"))
  getPolyhedraObject()$schedulePolyhedraSources(sources.config = sources.config,
                                      max.quant = max.quant.config.schedule)
  if (retry.scrape){
    mode <- "scrape.retry"
  }
  else {
    mode <- "scrape.queued"
  }
  futile.logger::flog.debug(paste("Scraping",max.quant.scrape, "polyhedra up to",time2scrape.source,"seconds"))
  getPolyhedraObject()$scrape(mode = mode, max.quant = max.quant.scrape,
                    time2scrape.source = time2scrape.source)
  #All files not scraped in building, marked as skipped
  still.queued <- which(getPolyhedraObject()$ledger$df$status=="queued")
  if (length(still.queued)>0){
    apply(getPolyhedraObject()$ledger$df[still.queued,], MARGIN = 1,
          FUN=function(x){getPolyhedraObject()$ledger$updateStatus(source = x["source"],
                                                         filename = x["filename"],
                                                         status = "skipped",
                                                         obs = "#TODO in next release")})
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
#'cubes <- getAvailablePolyhedra(sources=available.sources,search.string="cube")
getAvailableSources <- function(){
  getPolyhedraObject()$getAvailableSources()
}

#' getAvailablePolyhedra()
#'
#' Gets the list of names of available polyhedra and its status in the polyhedra database, which can be later
#' called with getPolyhedron
#'
#' @import futile.logger
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

getAvailablePolyhedra <- function(sources = names(getPackageEnvir(".available.sources")), search.string = NULL){
  getPolyhedraObject()$getAvailablePolyhedra(sources = sources, search.string = search.string)
}


#' getPercentilPolyhedraQuant
#' returns polyhedra quantity of parameter percentil
#' @param percentil is the percentil which must be applied to estimate the figure
#' @param quant.min minimum quantity of files to return
#'
getPercentilPolyhedraQuant <- function(percentil,quant.min=100){
  max(round(percentil*nrow(getAvailablePolyhedra())),quant.min)
}
#' getPolyhedron()
#'
#' Gets a polyhedron from the database. It returns an R6 Class with all its characteristics and functions.
#' The object returned, of type Polyhedron.class, allows to the user to get access to all the functionality provided.
#'
#' @param source source name
#' @param polyhedron.name  a valid name of a polyhedron in the database. Current names can be found with getAvailablePolyhedra()
#' @import futile.logger
#' @export
#' @return polyhedron R6 object
#' @export
#' @examples
#' tetrahedron <- getPolyhedron(source="netlib", polyhedron.name = 'tetrahedron')
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
  if (exists(".polyhedra", envir=getPackageEnvir("RpolyhedraEnv"))) {
    ret <- getPolyhedraObject()$getPolyhedron(source  =source, polyhedron.name = polyhedron.name)
  }
  ret
}


