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
      #TODO check polyhedron number
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
      #TODO remove geodesic files from proirity when everything works ok.
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

#' ScraperRecord
#' TODO add doc

ScraperRecord.class <- R6::R6Class("ScraperRecord",
  public = list(
    states = NULL,
    df = NA,
    preloaded.complexities.filename = NA,
    preloaded.complexities = NA,
    initialize = function() {
      self$df <- data.frame(id           = character(),
                            source       = character(),
                            number       = numeric(),
                            filename     = character(),
                            start.scrape = as.POSIXct(character()),
                            end.scrape   = as.POSIXct(character()),
                            status       = character(),
                            scraped.name = character(),
                            status.test  = character(),
                            obs          = character(),
                            git.commit   = character(),
                            time.scraped = numeric(),
                            preloaded.time2scrape = numeric(),
                            stringsAsFactors = FALSE)
      self$resetStatesMetrics()
      self$loadPreloadedComplexities()
      self
    },
    addFilename = function(source, filename){
      r <- NULL
      default.status <- "queued"
      if (is.null(self$getIdFilename(source, filename))){
        r <- nrow(self$df)+1
        status.field <- "status"
        self$countStatusUse(status.field = status.field, status = default.status)
        #TODO make abstraction as method
        states.row <- which(self$states$status.field %in% status.field &
                                    self$states$status %in% default.status)
        number <- self$states[states.row,"count"]

        #obtain preloaded.time2scrape
        row.preloaded.t2s <- which(self$preloaded.complexities$source == source &
                                     self$preloaded.complexities$filename==filename)
        if (length(row.preloaded.t2s)==1){
          preloaded.time2scrape <- as.numeric(self$preloaded.complexities[row.preloaded.t2s,"time2scrape"])
        }
        else{
          preloaded.time2scrape <- NA
        }
        self$df[r,c("id","source","filename","status")]<- c(r,source,filename,default.status)
        self$df[r,c("number","preloaded.time2scrape")]<- c(number,preloaded.time2scrape)
      }
      r
    },
    getIdFilename = function(source, filename){
      r <- which(self$df$source == source & self$df$filename == filename)
      if (length(r)>0){
        self$df[r,"id"]
      }
      else{
        r <- NULL
      }
      r
    },
    updateStatus = function(source, filename, status, status.field = "status",
                            scraped.name = NA, obs =""){
      if (is.null(obs)){
        obs <- ""
      }
      if (!status.field %in% c("status","status.test"))
        stop(paste("Cannot update invalid status field",status.field))
      ret <- NULL
      retrieved.id <- self$getIdFilename(source = source, filename = filename)
      if (length(retrieved.id)!=1){
        stop(paste("There must be a unique row for",source,filename,"and have",length(retrieved.id)))
      }
      if (status.field == "status"){
        end.scrape <- Sys.time()
        if (status == "scraping"){
          fields.update <- c("start.scrape")
          values.update <- as.character(end.scrape)
        }
        if (status %in% c("scraped","failed","skipped","exception")){
          fields.update <- c("end.scrape", "scraped.name", "git.commit","time.scraped")
          start.scrape  <- self$df[retrieved.id,"start.scrape"]
          time.scraped  <- round(as.numeric(end.scrape - start.scrape),3)
          values.update <- c(as.character(end.scrape), scraped.name, getGitCommit(),time.scraped)
        }
      }
      if (status.field == "status.test"){
        #status.test only possible value
        if (status %in% c("tested","testing","failed")){
          fields.update <- NULL
          values.update <- NULL
        }
      }

      fields.update  <- c(fields.update,status.field,"obs")
      values.update  <- c(values.update,status,obs)

      self$df[retrieved.id,fields.update] <- values.update
      ret <- self$df[retrieved.id,]
      #count status uses
      self$countStatusUse(status.field,status)
      ret
    },
    savePreloadedComplexities = function(){
      preloaded.complexities <- self$df[!is.na(self$df$time.scraped),c("source","filename","time.scraped")]
      preloaded.complexities <- preloaded.complexities[order(preloaded.complexities$time.scraped,
                                                             preloaded.complexities$source,
                                                             preloaded.complexities$filename),]
      names(preloaded.complexities)[3]<-"time2scrape"
      write.csv(preloaded.complexities, self$preloaded.complexities.filename,
                row.names = FALSE)
      preloaded.complexities
    },
    loadPreloadedComplexities = function(){
      self$preloaded.complexities.filename <- paste(getDataDir(),"polyhedra.complexity.csv",sep="")
      self$preloaded.complexities <- read.csv(paste(getDataDir(),"polyhedra.complexity.csv",sep=""))
      self$preloaded.complexities
    },
    getSizeToTimeScrape = function(sources, time2scrape = 60){
      pre.comp.source <- self$preloaded.complexities[self$preloaded.complexities$source %in% sources,]
      pre.comp.source <- pre.comp.source[order(pre.comp.source$time2scrape,
                                               pre.comp.source$source,
                                               pre.comp.source$filename),]
      pre.comp.source$cummsum <- cumsum(pre.comp.source$time2scrape)
      length(which(pre.comp.source$cummsum<time2scrape))
    },
    resetStatesMetrics = function(){
      self$states <- data.frame(status.field = character(),
                                status       = character(),
                                count        = numeric(),
                                stringsAsFactors = FALSE)
      self$states
    },
    countStatusUse = function(status.field,status){
      status.row <- which(self$states$status.field %in% status.field &
                          self$states$status %in% status)
      if (length(status.row)==0){
        status.row <- nrow(self$states) + 1
        count <- 1
      }
      else {
        count <- self$states[status.row,"count"]+1
      }
      self$states[status.row, c("status.field","status")] <- c(status.field,status)
      self$states[status.row, "count"] <- count
      self$states
    },
    getFilenamesStatusMode = function(mode,
                                      sources = sort(unique(self$df$source)),
                                      max.quant = 0,
                                      order.by.time2scrape = FALSE){
      #status in queued, scraped, exception, retry, skipped
      allowed.status<- NULL
      if (mode == "scrape.retry"){
        allowed.status <- c("queued","exception","failed")
      }
      if (mode == "scrape.queued"){
        allowed.status <- c("queued")
      }
      if (mode == "test"){
        allowed.status <- "scraped"
      }
      if (mode == "skipped"){
        allowed.status <- c("skipped","scraping")
      }
      self$getFilenamesStatus(status = allowed.status,
                              sources = sources,
                              max.quant = max.quant,
                              order.by.time2scrape = order.by.time2scrape)
    },
    getFilenamesStatus = function(status,
                                  sources = sort(unique(self$df$source)),
                                  max.quant = 0,
                                  order.by.time2scrape = FALSE){
      filtred.rows <- which(self$df$source %in% sources &
                            self$df$status %in% status)
      ret <- NULL
      if (length(filtred.rows)>0){
        if (max.quant >0){
          filtred.rows <- filtred.rows[1:min(max.quant,length(filtred.rows))]
        }
        ret <- self$df[filtred.rows,]
        if (order.by.time2scrape){
          ret <- ret[order(ret$preloaded.time2scrape,ret$source,ret$number),]
        }
      }
      ret
    }
  ))

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
      # TODO debug inferEdges not working while scraping

      #debug gp
      #print(scraped.polyhedron)
      #print(expected.polyhedron)
      #expected.polyhedron <<- expected.polyhedron
      #task <<- self

      expect_equal(scraped.name, self$polyhedron.name)
      expect_equal(scraped.polyhedron, expected.polyhedron)

    }))


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
#' @field record rr record of scraping process
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
    record         = NA,
    data           = NA,
    initialize = function() {
      self$record         <- ScraperRecord.class$new()
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
      self$record$updateStatus(source = source,filename = polyhedron.filename,
                               status = "scraped",scraped.name = polyhedron.name)
      polyhedron
    },
    configPolyhedraSource = function(source.config, max.quant = 0) {
      source <- source.config$getName()
      home.dir.data <- getDataDir()
      self$configPolyhedraRDSPath()
      futile.logger::flog.debug(paste("opening", self$polyhedra.rds.file))
      polyhedra.dir   <- source.config$getBaseDir(home.dir.data)
      polyhedra.files <- source.config$getPolyhedraFiles(home.dir.data)
      if (max.quant >0){
        polyhedra.files <- polyhedra.files[1:min(max.quant,length(polyhedra.files))]
      }
      if (length(polyhedra.files)>0) {
        self$addSourceConfig(source.config)
        scheduled <- NULL
        for (polyhedron.filename in polyhedra.files) {
          if (is.null(self$record$getIdFilename(source = source,filename = polyhedron.filename))){
            self$record$addFilename(source = source, filename = polyhedron.filename)
            scheduled <- c(scheduled, polyhedron.filename)
          }
        }
        if (length(scheduled)>0){
          futile.logger::flog.info(paste("Scheduling source ",source, "filenames:",
                                         paste(scheduled, collapse = ",")))
        }

      }
      saveRDS(self, self$polyhedra.rds.file)
      self
    },
    cover = function(mode,
                     sources = names(self$sources.config),
                     covering.code,
                     max.quant=0){
      self$configPolyhedraRDSPath()
      #TODO include
      #source.config$initScrape()
      # For correct poyhedron numbering while testing

      filenames2scrape <- self$record$getFilenamesStatusMode(mode = mode,
                                                             sources = sources,
                                                             max.quant = max.quant,
                                                             order.by.time2scrape = TRUE)
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
        saveRDS(self, self$polyhedra.rds.file)
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
          self$record$updateStatus(source = source,filename = polyhedron.filename,
                                 status = "scraping",scraped.name = polyhedron.name)
          current.polyhedron <- source.config$scrape(polyhedron.number = polyhedron.number, paste(polyhedra.dir, polyhedron.filename, sep = ""))
          if (current.polyhedron$isChecked()){
            current.polyhedron$getRGLModel(1, c(0, 0, 0))
            futile.logger::flog.debug(paste("generated RGLModel"))
            self$addPolyhedron(source=source, polyhedron.filename = polyhedron.filename,
                               polyhedron = current.polyhedron)
          }
          else{
            errors <- current.polyhedron$getErrors()
            self$record$updateStatus(source,polyhedron.filename,status = "failed",obs=errors)
          }
        },
        error=function(e){
          error <- paste(e$message,collapse=",")
          futile.logger::flog.error(paste("catched error",error))
          assign("error",error,envir = parent.env(environment()))
          self$record$updateStatus(source,polyhedron.filename,status = "exception",obs=error)
        })
        current.polyhedron
      }
      if (time2scrape.source >0){
        max.quant.time <- self$record$getSizeToTimeScrape(sources = sources, time2scrape = time2scrape.source)
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

      #TODO change to debug
      futile.logger::flog.info(paste("Scraping sources",paste(sources,collapse=","),
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
        self$record$updateStatus(source,polyhedron.filename, status.field = "status.test", status = status, obs= obs)
        if (status=="testing"){
          tryCatch({
            res <- expect_true(self$existsPolyhedron(source = source,
                                              polyhedron.name = polyhedron.name))
            db.polyhedron <- self$getPolyhedron(source = source,
                                                polyhedron.name = polyhedron.name)
            #debug
            #scraped.polyhedron <<- scraped.polyhedron
            #db.polyhedron <<- db.polyhedron
            # TODO debug inferEdges not working while scraping
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
          self$record$updateStatus(source,polyhedron.filename, status.field = "status.test", status = status, obs= obs)
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
        polyhedron.record <- polyhedra.db.saved$record$df[polyhedra.db.saved$record$getIdFilename(source,polyhedron.filename),]
        polyhedron.name   <- polyhedron.record$scraped.name

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
    getAvailablePolyhedra = function(sources = getAvailableSources(),
                                     search.string = NULL,ignore.case=TRUE) {
      ret <- NULL
      for (source in sources){
        source.polyhedra <- self$getSource(source)
        if (!is.null(source.polyhedra)){
          if (!is.null(search.string)) {
            source.polyhedra.names <- names(source.polyhedra)
            polyhedron.names <- source.polyhedra.names[grepl(search.string, source.polyhedra.names,ignore.case = ignore.case)]
          } else {
            polyhedron.names <- names(source.polyhedra)
          }
          if (length(polyhedron.names)>0){
            current.ret <- data.frame(source=source, polyhedron.name = filtred.polyhedra.names,
                                      stringsAsFactors = FALSE)
            ret <- rbind(ret,current.ret)
          }
        }
        else{
          stop(paste("No polyhedra found for source",source))
        }
      }
      ret
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
  still.queued <- which(.polyhedra$record$df$status=="queued")
  .polyhedra$record$df[still.queued,"status"] <- "skipped"
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
#' Gets the list of names of available polyhedra in the internal database, which can be later
#' called with getAvailablePolyhedra for later use.
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
