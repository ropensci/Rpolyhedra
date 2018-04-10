#' Gets the path of package data.
#'
#' This function is used internally to determine whether the package
#' is compiled in source or package directory.
getDataDir <- function() {
  home.dir <- find.package("Rpolyhedra", lib.loc = NULL, quiet = TRUE)
  data.subdir <- "inst/extdata/"
  if (!dir.exists(paste(home.dir, "/", data.subdir, sep = "")))
    data.subdir <- "extdata/"
  paste(home.dir, "/", data.subdir, sep = "")
}

#' Gets the path of Polyhedra RDS database file
#'
#' @param polyhedra_rds_filename filename of polyhedra database
#' @return the path to the Polyhedra database file
getPolyhedraRDSPath <- function(polyhedra_rds_filename = "polyhedra.RDS") {
  paste(getDataDir(), polyhedra_rds_filename, sep = "")
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
      paste(home.dir.data,self$base.dir,sep="")
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
      geodesic.files  <- polyhedra.files[grep("geodesic",polyhedra.files,ignore.case = TRUE)]
      geodesic.files  <- geodesic.files[-grep("Dual",geodesic.files,ignore.case = TRUE)]
      regexp.natnum   <- "([0-9]+)"
      geodesic.order  <- data.frame(filename = geodesic.files,
                                    class= sub(regexp.natnum,"",geodesic.files),
                                    order= as.numeric(str_extract(geodesic.files,regexp.natnum)),
                                    stringsAsFactors = FALSE)
      geodesic.order  <- geodesic.order[order(geodesic.order$order,geodesic.order$class),]
      geodesic.order  <- geodesic.order[1:50,]
      non.geodesic.files <- polyhedra.files[-which(polyhedra.files %in% geodesic.order$filename)]
      polyhedra.files <- c(geodesic.order$filename,non.geodesic.files[order(non.geodesic.files)])
      #TODO remove geodesic files from priority when everything works ok.
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
        scraped.polyhedron <- self$source.config$scrape(polyhedron.number = self$polyhedron.number,
                                                   paste(self$polyhedra.dir, self$polyhedron.filename, sep = ""))
        scraped.name <- scraped.polyhedron$getName()
        scraped.polyhedron$getRGLModel(1, c(0, 0, 0))
        futile.logger::flog.debug(paste("generated RGLModel"))
        status <- "testing"
        obs    <- ""
      },
      error=function(e){
        error <- paste(e$message,collapse=",")
        futile.logger::flog.error(paste("catched error",error))
        assign("error",error,envir = parent.env(environment()))
        status <- "exception"
        obs    <- scraped.polyhedron$getErrors()
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



#' PolyhedronDatabase
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
PolyhedronDatabase.class <- R6::R6Class("PolyhedronDatabase",
  public = list(
    polyhedra.rds.file = NA,
    sources.config = NA,
    ledger         = NA,
    data           = NA,
    initialize = function() {
      self$ledger         <- ScraperLedger.class$new()
      self$sources.config <- list()
      self$data           <- list()
      self
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
      if (max.quant >0){
        polyhedra.files <- polyhedra.files[1:min(max.quant,length(polyhedra.files))]
      }
      if (length(polyhedra.files)>0) {
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
                     max.quant=0){
      self$configPolyhedraRDSPath()
      filenames2scrape <- self$ledger$getFilenamesStatusMode(mode = mode,
                                                             sources = sources,
                                                             max.quant = max.quant,
                                                             order.by.vertices.faces = TRUE)
      ret <- list()
      home.dir.data <- getDataDir()
      if (!is.null(filenames2scrape)){
        for (r in c(1:nrow(filenames2scrape))){
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
        #FIXME add exception catching
        #tryCatch({
          self$ledger$updateStatus(source = source,filename = polyhedron.filename,
                                 status = "scraping")
          current.polyhedron <- source.config$scrape(polyhedron.number = polyhedron.number, paste(polyhedra.dir, polyhedron.filename, sep = ""))
          if (current.polyhedron$isChecked()){
            current.polyhedron$getRGLModel(transformation.matrix = identityMatrix())
            futile.logger::flog.debug(paste("generated RGLModel"))
            self$addPolyhedron(source=source, polyhedron.filename = polyhedron.filename,
                               polyhedron = current.polyhedron)
          }
          else{
            errors <- current.polyhedron$getErrors()
            self$ledger$updateStatus(source,polyhedron.filename,status = "failed",obs=errors)
          }
        # },
        # error=function(e){
        #   error <- paste(e$message,collapse=",")
        #   futile.logger::flog.error(paste("catched error",error))
        #   assign("error",error,envir = parent.env(environment()))
        #   self$ledger$updateStatus(source,polyhedron.filename,status = "exception",obs=error)
        # })
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
        if (!compatiblePolyhedraRDS(polyhedra.db.saved)){
          stop("Incompatible polyhedra.db saved. Contact package mantainer")
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
          scraped.polyhedron <- source.config$scrape(polyhedron.number = polyhedron.number, paste(polyhedra.dir, polyhedron.filename, sep = ""))
          polyhedron.name <- scraped.polyhedron$getName()
          status <- "testing"
          obs    <- ""
        },
        error=function(e){
          error <- paste(e$message,collapse=",")
          futile.logger::flog.error(paste("catched error",error))
          assign("error",error,envir = parent.env(environment()))
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
            assign("error",error,envir = parent.env(environment()))
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
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!compatiblePolyhedraRDS(polyhedra.db.saved)){
          stop("Incompatible polyhedra.db saved. Contact package mantainer")
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
                        max.quant     = max.quant)
      ret
    },
    schedulePolyhedraSources=function (sources.config=.available.sources,max.quant = 0, test = FALSE){
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

#' compatiblePolyhedraRDS()
#'
#' Tests if the polyhedra RDS is compatible with the current format
#'
#' @param .polyhedra current polyhedra database
compatiblePolyhedraRDS <- function(.polyhedra = .polyhedra){
  file.class <- class(.polyhedra)
  compatible <- FALSE
  if (file.class[[1]]=="PolyhedronDatabase"){
    compatible <- TRUE
  }
  compatible
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
#' @usage
#'     scrapePolyhedraSources(sources.config = .available.sources, max.quant.config.schedule = 0,
#'     max.quant.scrape = 0, time2scrape.source = 30, retry.scrape = FALSE)
#' @export
scrapePolyhedraSources<- function(sources.config = .available.sources,
                                  max.quant.config.schedule = 0,
                                  max.quant.scrape = 0,
                                  time2scrape.source = 30,
                                  retry.scrape = FALSE){
  .polyhedra$schedulePolyhedraSources(sources.config = sources.config,
                                      max.quant = max.quant.config.schedule)
  if (retry.scrape){
    mode <- "scrape.retry"
  }
  else {
    mode <- "scrape.queued"
  }
  .polyhedra$scrape(mode = mode, max.quant = max.quant.scrape,
                    time2scrape.source = time2scrape.source)
  #All files not scraped in building, marked as skipped
  still.queued <- which(.polyhedra$ledger$df$status=="queued")
  if (length(still.queued)>0){
    apply(.polyhedra$ledger$df[still.queued,], MARGIN = 1,
          FUN=function(x){.polyhedra$ledger$updateStatus(source = x["source"],
                                                         filename = x["filename"],
                                                         status = "skipped",
                                                         obs = "#TODO in next release")})
    #save skipped state in RDS file
    .polyhedra$saveRDS()
  }
  .polyhedra
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
  .polyhedra$getAvailableSources()
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

getAvailablePolyhedra <- function(sources = names(.available.sources), search.string = NULL){
  .polyhedra$getAvailablePolyhedra(sources = sources, search.string = search.string)
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
  if (exists(".polyhedra")) {
    ret <- .polyhedra$getPolyhedron(source  =source, polyhedron.name = polyhedron.name)
  }
  ret
}
