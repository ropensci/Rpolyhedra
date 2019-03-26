#' Polyhedra database
#'
#' Scrapes all polyhedra in data folder to save a representation which
#' is accessible by the final users upon call to \code{getPolyhedron()}.
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
#'   \item{\code{addPolyhedron(source, polyhedron, overwrite, save.on.change = FALSE)}}{Adds a polyhedron
#'   by source and name, if overwrite is TRUE, it will update any existing one
#'   by that source and name}
#'   \item{\code{configPolyhedraSource(source.config, source.filenames, max.quant)}}{Scrapes all
#'   polyhedra in the given directory for adding to db or testing}
#'   \item{\code{schedulePolyhedraSources(sources.config, source.filenames, max.quant,
#'   test)}}{Scrapes files applying parameter sources.config}
#'   \item{\code{cover(sources, covering.code, polyhedra.names = NULL,
#'                     max.quant = 0, save.on.change = FALSE, seed = NULL)}}{Cover all polyhedron with specified code}
#'   \item{\code{scrape(mode = "scrape.queued",
#'                      sources = names(self$sources.config),
#'                      max.quant = 0, time2scrape.source = 30,
#'                      save.on.change = FALSE, skip.still.queued = FALSE)}}{Scrape file with specified parameters}
#'   \item{\code{saveRDS = function(save.on.change = TRUE)}}{Save state in file when specified}
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
                             save.on.change = FALSE) {
      polyhedron.name <- polyhedron$getName()
      data.dir <- self$getPolyhedraSourceDir(source = source)
      prev.data <- self$getPolyhedron(source = source,
                                      polyhedron.name = polyhedron.name)
      if (!overwrite & !is.null(prev.data) & !save.on.change){
        futile.logger::flog.info(paste("Polyhedron",
                                     polyhedron.name,
                                     "in source",
                                     source,
                                     "already in database"))
      }
      else {
        crc.name <- self$ledger$getCRCPolyhedronName(source = source,
               polyhedron.name = polyhedron.name)
        serialized.polyhedron <- polyhedron$state$serialize()
        tmp.dir <- file.path(tempdir = tempdir(), source)
        dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
        serialized.filename <- paste(crc.name, ".RDS", sep = "")
        tmp.filename <- file.path(tmp.dir, serialized.filename)
        if (save.on.change){
          saveRDS(object = serialized.polyhedron, ascii = TRUE,
                  file = tmp.filename)
          zip(zipfile = file.path(data.dir, paste(crc.name,
                                                  ".RDS.zip",
                                                  sep = "")),
              files = tmp.filename, flags = "-j")
          unlink(tmp.filename)
        }
        futile.logger::flog.info(paste(ifelse(save.on.change,
                                              "[save.on.change]", ""),
                                       "Added polyhedron in file",
                                       polyhedron.name,
                                       "#|n",
                                       polyhedron$file.id,
                                       polyhedron.name,
                                       "in source",
                                       source,
                                       "to database with CRC",
                                       crc.name))
      }
      self$ledger$updateStatus(source = source,
                               source.filename = source.filename,
                               status = "scraped",
                               scraped.polyhedron = polyhedron)
      polyhedron
    },
    configPolyhedraSource = function(source.config, source.filenames= NULL,
                                     max.quant = 0,
                                     save.on.change = FALSE) {
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
      self$saveRDS(save.on.change = save.on.change)
      self
    },
    saveRDS = function(save.on.change = TRUE){
      ret <- NULL
      if (self$ledger$dirty & save.on.change){
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
                     save.on.change = FALSE,
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
        self$saveRDS(save.on.change = save.on.change)
      }
      ret
    },
    scrape = function(mode = "scrape.queued",
                      sources = names(self$sources.config),
                      max.quant = 0,
                      time2scrape.source = 30,
                      save.on.change = FALSE,
                      skip.still.queued = FALSE){
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
                               polyhedron = current.polyhedron,
                               save.on.change = save.on.change)
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
      ret <- self$cover(mode    = mode,
                 sources        = sources,
                 covering.code  = scrape.function,
                 max.quant      = max.quant.scrape,
                 save.on.change = save.on.change)
      if (skip.still.queued){
        #All files not scraped in building, marked as skipped
        still.queued <- which(self$ledger$df$status == "queued")
        if (length(still.queued) > 0){
          apply(self$ledger$df[still.queued, ], MARGIN = 1,
                FUN = function(x) {
                  self$ledger$
                    updateStatus(source = x["source"],
                                 source.filename = x["source.filename"],
                                 status = "skipped",
                                 obs = "#TODO in next release")
                }
          )
          #save skipped state in RDS file
          self$saveRDS(save.on.change = save.on.change)
        }
      }
      ret
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
            res <- testthat::expect_true(self$existsPolyhedron(source = source,
                      polyhedron.name = polyhedron.name))
            db.polyhedron <- self$getPolyhedron(source = source,
                      polyhedron.name = polyhedron.name)
            scraped.polyhedron$state$inferEdges()
            testthat::expect_equal(scraped.polyhedron, db.polyhedron)
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
                        seed            = seed)
      ret
    },
    schedulePolyhedraSources = function (sources.config =
                                         getPackageEnvir(".available.sources"),
                                         source.filenames= NULL,
                                         max.quant = 0,
                                         test = FALSE,
                                         save.on.change = FALSE){
      for (source  in names(sources.config)){
        self$configPolyhedraSource(source.config    = sources.config[[source]],
                                   source.filenames = source.filenames,
                                   max.quant        = max.quant,
                                   save.on.change   = save.on.change)
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
