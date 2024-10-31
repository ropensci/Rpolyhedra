#' Polyhedra database
#'
#' @description
#' Scrapes all polyhedra in data folder to save a representation which
#' is accessible by the final users upon call to \code{getPolyhedron()}.
#'
#' @docType class
#' @import lgr
#' @importFrom utils zip
#' @importFrom R6 R6Class
PolyhedraDatabase <- R6::R6Class("PolyhedraDatabase",
  public = list(
    #' @field version version of database file
    version = NA,
    #' @field polyhedra.rds.file path of rds database file
    polyhedra.rds.file = NA,
    #' @field sources.config Sources configuration for scraping different sources
    sources.config = NA,
    #' @field ledger rr ledger of scraping process
    ledger = NA,
    #' @field logger class logger
    logger = NA,
    #' @description
    #' Create a new PolyhedraDatabase object.
    #' @return A new `PolyhedraDatabase` object.
    initialize = function() {
      self$version <- getDatabaseVersion()
      self$ledger <- ScraperLedger$new()
      self$sources.config <- list()
      self$logger <- genLogger(self)
      self
    },
    #' @description
    #' get the version of the current object.
    #' @return Database version
    getVersion = function() {
      self$version
    },
    #' @description
    #' sets the path of the RDS object
    #' @return Database version
    configPolyhedraRDSPath = function() {
      self$polyhedra.rds.file <- getPolyhedraRDSPath()
      self$polyhedra.rds.file
    },
    #' @description
    #' Determines if the source exists on
    #'   the database
    #' @param source source description
    #' @return boolean value
    existsSource = function(source) {
      source %in% self$getAvailableSources()
    },
    #' @description
    #' add  source.config to the database
    #' @param source.config SourceConfig object able to scrape source polyhedra definitions
    #' @return PolyhedraDatabase object
    addSourceConfig = function(source.config) {
      source <- source.config$getName()
      if (!self$existsSource(source)) {
        self$sources.config[[source]] <- source.config
        self$getPolyhedraSourceDir(source)
      }
      self
    },
    #' @description
    #' Determines if the database includes a polyhedron which name
    #' matches the parameter value
    #' @param source source description
    #' @param polyhedron.name polyhedron description
    #'
    #' @return boolean value
    existsPolyhedron = function(source = "netlib", polyhedron.name) {
      ret <- FALSE
      file.path <- self$getPolyhedronFilename(
        source = source,
        polyhedron.name = polyhedron.name,
        extension = ".RDS.zip"
      )
      ret <- !is.null(file.path)
      if (ret) {
        ret <- file.exists(file.path)
      }
      ret
    },
    #' @description
    #' gets polyhedra sources folder
    #' @param source source description
    #' @param create.dir if dir does not exists, create it
    #' @return string with polyhedra sources path
    getPolyhedraSourceDir = function(source, create.dir = TRUE) {
      ret <- file.path(getDataDir(), "polyhedra", source, "/")
      if (create.dir) {
        dir.create(ret, showWarnings = FALSE, recursive = TRUE)
      }
      ret
    },
    #' @description
    #' gets the filename of the polyhedron matching parameter.
    #' @param source source description
    #' @param polyhedron.name polyhedron description
    #' @param extension extension of the polyhedron filename
    #' @return string with polyhedron filename
    getPolyhedronFilename = function(source, polyhedron.name, extension) {
      paste(self$getPolyhedraSourceDir(source),
        self$ledger$getCRCPolyhedronName(
          source = source,
          polyhedron.name = polyhedron.name
        ),
        extension,
        sep = ""
      )
    },
    #' @description
    #' gets polyhedron object which name
    #' matches the parameter value
    #' @param source source description
    #' @param polyhedron.name polyhedron description
    #' @param strict halts execution if polyhedron not found
    #' @return Polyhedron object
    getPolyhedron = function(source = "netlib", polyhedron.name,
                             strict = FALSE) {
      data.dir <- self$getPolyhedraSourceDir(source = source)
      if (!self$existsPolyhedron(
        source = source, polyhedron.name =
          polyhedron.name
      )) {
        message <- paste(
          "Polyhedron", polyhedron.name,
          "not available in source", source
        )
        if (strict) {
          stop(message)
        }
      }
      ret <- NULL
      crc.name <- self$ledger$getCRCPolyhedronName(
        source = source,
        polyhedron.name = polyhedron.name
      )
      zip.filename <- file.path(data.dir, paste(crc.name, ".RDS.zip", sep = ""))
      serialized.polyhedron <- NULL
      if (file.exists(zip.filename)) {
        tmp.dir <- file.path(tempdir = tempdir(), source)
        dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
        serialized.filename <- paste(crc.name, ".RDS", sep = "")
        tmp.filename <- file.path(tmp.dir, serialized.filename)
        unzip(
          zipfile = zip.filename, files = serialized.filename,
          exdir = tmp.dir
        )
        serialized.polyhedron <- readRDS(file = tmp.filename)
        unlink(tmp.filename)
      }
      if (!is.null(serialized.polyhedron)) {
        ret <- Polyhedron$new(file.id = NA)
        ret$deserialize(serialized.polyhedron = serialized.polyhedron)
      }
      ret
    },
    #' @description
    #' add polyhedron object to the database
    #' @param source source description
    #' @param source.filename filename of the polyhedron source definition
    #' @param polyhedron polyhedron object
    #' @param overwrite overwrite exiting definition
    #' @param save.on.change saves Database state after operation
    #' @return Polyhedron object
    addPolyhedron = function(source = "netlib", source.filename,
                             polyhedron, overwrite = FALSE,
                             save.on.change = FALSE) {
      logger <- getLogger(self)
      polyhedron.name <- polyhedron$getName()
      data.dir <- self$getPolyhedraSourceDir(source = source)
      prev.data <- self$getPolyhedron(
        source = source,
        polyhedron.name = polyhedron.name
      )
      if (!overwrite & !is.null(prev.data) & !save.on.change) {
        logger$info(
          "Polyhedron",
          polyhedron.name = polyhedron.name,
          source = source,
          message = "already in database"
        )
      } else {
        crc.name <- self$ledger$getCRCPolyhedronName(
          source = source,
          polyhedron.name = polyhedron.name
        )
        serialized.polyhedron <- polyhedron$state$serialize()
        tmp.dir <- file.path(tempdir = tempdir(), source)
        dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
        serialized.filename <- paste(crc.name, ".RDS", sep = "")
        tmp.filename <- file.path(tmp.dir, serialized.filename)
        if (save.on.change) {
          saveRDS(
            object = serialized.polyhedron, ascii = TRUE,
            file = tmp.filename
          )
          zip(
            zipfile = file.path(data.dir, paste(crc.name,
              ".RDS.zip",
              sep = ""
            )),
            files = tmp.filename, flags = "-j"
          )
          unlink(tmp.filename)
        }
        logger$info(
          "Added polyhedron in file",
          save.on.change = save.on.change,
          polyhedron.name = polyhedron.name,
          file.id = polyhedron$file.id,
          source = source,
          crc.name = crc.name
        )
      }
      self$ledger$updateStatus(
        source = source,
        source.filename = source.filename,
        status = "scraped",
        scraped.polyhedron = polyhedron
      )
      polyhedron
    },
    #' @description
    #' Process parameter filenames using source.config parameter
    #' @param source.config source configuration for scraping files
    #' @param source.filenames filenames of the polyhedron source definition
    #' @param max.quant maximum filenames to process
    #' @param save.on.change saves Database state after operation
    #' @return Modified `PolyhedraDatabase` object.
    configPolyhedraSource = function(source.config, source.filenames = NULL,
                                     max.quant = 0,
                                     save.on.change = FALSE) {
      logger <- getLogger(self)
      source <- source.config$getName()
      home.dir.data <- getDataDir()
      self$configPolyhedraRDSPath()
      logger$debug("Configuring source", source = source)
      polyhedra.dir <- source.config$getBaseDir(home.dir.data)
      polyhedra.files <- source.config$getPolyhedraFiles(home.dir.data)
      if (!is.null(source.filenames)) {
        polyhedra.files <- polyhedra.files[
          polyhedra.files %in% source.filenames
        ]
      }
      if (length(polyhedra.files) > 0) {
        if (max.quant > 0) {
          polyhedra.files <- polyhedra.files[1:min(
            max.quant,
            length(polyhedra.files)
          )]
        }
        self$addSourceConfig(source.config)
        scheduled <- NULL
        for (source.filename in polyhedra.files) {
          if (is.null(self$ledger$getIdFilename(
            source = source,
            source.filename = source.filename
          ))) {
            self$ledger$addFilename(
              source = source,
              source.filename = source.filename
            )
            scheduled <- c(scheduled, source.filename)
          }
        }
        if (length(scheduled) > 0) {
          logger$info(
            "Scheduling source ",
            source = source,
            scheduled = length(scheduled),
            filenames = paste(scheduled, collapse = ",")
          )
        }
      }
      self$saveRDS(save.on.change = save.on.change)
      self
    },
    #' @description
    #' saveRDS
    #' @param save.on.change saves Database state after operation
    #' @return saveRDS return status
    saveRDS = function(save.on.change = TRUE) {
      logger <- getLogger(self)
      ret <- NULL
      if (self$ledger$dirty & save.on.change) {
        self$ledger$updateCalculatedFields()
        logger$info(
          "Saving RDS in file",
          rds.file = self$polyhedra.rds.file
        )
        ret <- saveRDS(self, self$polyhedra.rds.file)
      }
      ret
    },
    #' @description
    #' Cover objects and applies covering.code parameter
    #' @param mode covering mode. Available values are "scrape.queued", "scrape.retry","skipped",  "test"
    #' @param sources sources names
    #' @param covering.code code for applying in covering
    #' @param polyhedra.names polyhedra names to cover (optional)
    #' @param max.quant maximum numbers of polyhedra to cover
    #' @param save.on.change saves Database state after operation
    #' @param seed seed for deterministic random generator
    #' @return A list with resulting objects covered
    cover = function(mode,
                     sources = names(self$sources.config),
                     covering.code,
                     polyhedra.names = NULL,
                     max.quant = 0,
                     save.on.change = FALSE,
                     seed = NULL) {
      self$configPolyhedraRDSPath()
      if (!is.null(seed)) {
        set.seed(seed)
        max.quant.retrieve <- 0
      } else {
        max.quant.retrieve <- max.quant
      }
      filenames2scrape <- self$ledger$getFilenamesStatusMode(
        mode = mode,
        sources = sources,
        max.quant = max.quant.retrieve,
        order.by.vertices.faces = TRUE
      )
      if (!is.null(polyhedra.names)) {
        filenames2scrape <- filenames2scrape[filenames2scrape$scraped.name
          %in% polyhedra.names, ]
      }

      ret <- list()
      home.dir.data <- getDataDir()
      if (!is.null(filenames2scrape)) {
        if (!is.null(seed)) {
          n <- nrow(filenames2scrape)
          if (max.quant < n) {
            sample.2.cover <- sort(sample(1:n, size = max.quant))
          } else {
            sample.2.cover <- 1:n
          }
          filenames2scrape <- filenames2scrape[sample.2.cover, ]
        }
        for (r in seq_len(nrow(filenames2scrape))) {
          current.filename.data <- filenames2scrape[r, ]

          source <- current.filename.data$source
          source.config <- self$sources.config[[source]]
          polyhedra.dir <- source.config$getBaseDir(home.dir.data)

          polyhedron.file.id <- current.filename.data$file.id
          # file.id should have a value
          stopifnot(!is.na(polyhedron.file.id))

          source.filename <- current.filename.data$source.filename
          ret[[paste(source, source.filename, sep = "|")]] <-
            covering.code(
              polyhedra.dir = polyhedra.dir,
              source.config = source.config,
              polyhedron.file.id = polyhedron.file.id,
              source.filename = source.filename
            )
        }
        # after covering, save RDS
        self$saveRDS(save.on.change = save.on.change)
      }
      ret
    },
    #' @description
    #' Scrape polyhedra queued sources
    #' @param mode covering mode. Available values are "scrape.queued", "scrape.retry","skipped",  "test"
    #' @param sources sources names
    #' @param covering.code code for applying in covering
    #' @param polyhedra.names polyhedra names to cover (optional)
    #' @param max.quant maximum numbers of polyhedra to cover
    #' @param time2scrape.source maximum time to spend scraping each source
    #' @param save.on.change saves Database state after operation
    #' @param skip.still.queued Flag unscraped files with status `skipped``
    #' @return A list with resulting objects covered
    scrape = function(mode = "scrape.queued",
                      sources = names(self$sources.config),
                      max.quant = 0,
                      time2scrape.source = 30,
                      save.on.change = FALSE,
                      skip.still.queued = FALSE) {
      logger <- getLogger(self)
      scrape.function <- function(polyhedra.dir, source.config,
                                  polyhedron.file.id, source.filename) {
        source <- source.config$getName()
        current.polyhedron <- NULL
        tryCatch(
          {
            self$ledger$updateStatus(
              source = source,
              source.filename = source.filename,
              status = "scraping"
            )

            source.filename.path <- file.path(
              polyhedra.dir,
              source.filename
            )

            current.polyhedron <- source.config$scrape(
              polyhedron.file.id = polyhedron.file.id,
              source.filename = source.filename.path
            )
            if (current.polyhedron$isChecked()) {
              self$addPolyhedron(
                source = source,
                source.filename = source.filename,
                polyhedron = current.polyhedron,
                save.on.change = save.on.change
              )
            } else {
              errors <- current.polyhedron$getErrors()
              self$ledger$updateStatus(
                source = source,
                source.filename = source.filename,
                status = "failed", obs = errors
              )
            }
          },
          error = function(e) {
            #browser()
            logger <- getLogger(self)
            error <- paste(e$message, e$call, collapse = ",")
            logger$error("Caught error",
                         error = error,
                         source = source,
                         filename = source.filename
                         )
            assign("error", error, envir = parent.env(environment()))
            self$ledger$updateStatus(
              source = source,
              source.filename,
              status = "exception",
              obs = error
            )
          }
        )
        current.polyhedron
      }
      if (time2scrape.source > 0) {
        max.quant.time <- self$ledger$getSizeToTimeScrape(
          sources = sources,
          time2scrape = time2scrape.source
        )
      } else {
        max.quant.time <- 0
      }
      if (max.quant > 0 & max.quant.time > 0) {
        max.quant.scrape <- min(max.quant, max.quant.time)
      } else {
        # if 1 parameters is 0, then max for not executing 0
        max.quant.scrape <- max(
          max.quant,
          max.quant.time
        )
      }
      logger$debug(
        "Scraping sources",
        sources = paste(sources, collapse = ","),
        max.quant = max.quant,
        time2scrape.source = time2scrape.source,
        max.quant.time = max.quant.time,
        max.quant.scrape = max.quant.scrape
      )
      ret <- self$cover(
        mode = mode,
        sources = sources,
        covering.code = scrape.function,
        max.quant = max.quant.scrape,
        save.on.change = save.on.change
      )
      if (skip.still.queued) {
        # All files not scraped in building, marked as skipped
        still.queued <- which(self$ledger$df$status == "queued")
        if (length(still.queued) > 0) {
          apply(self$ledger$df[still.queued, ],
            MARGIN = 1,
            FUN = function(x) {
              self$ledger$
                updateStatus(
                source = x["source"],
                source.filename = x["source.filename"],
                status = "skipped",
                obs = "#TODO in next release"
              )
            }
          )
          # save skipped state in RDS file
          self$saveRDS(save.on.change = save.on.change)
        }
      }
      ret
    },
    #' @description
    #' testRR
    #' @param sources sources names
    #' @param max.quant maximum numbers of polyhedra to cover
    #' @return A list with resulting objects tested
    testRR = function(sources = names(self$sources.config),
                      max.quant = 0) {
      logger <- getLogger(self)
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!isCompatiblePolyhedraRDS(polyhedra.db.saved)) {
          stop("Incompatible polyhedra.db saved. Contact package maintainer.")
        }
      } else {
        # if database doesn't exists setup test as false
        logger$error(paste(
          "There is no polyhedra database so ",
          "test could not be ran"
        ))
        test <- FALSE
      }
      test.function <- function(polyhedra.dir,
                                source.config,
                                polyhedron.file.id,
                                source.filename) {
        logger <- getLogger(self)
        source <- source.config$getName()
        scraped.polyhedron <- NULL
        tryCatch(
          {
            scraped.polyhedron <- source.config$scrape(
              polyhedron.file.id = polyhedron.file.id,
              source.filename = file.path(polyhedra.dir, source.filename)
            )
            polyhedron.name <- scraped.polyhedron$getName()
            status <- "testing"
            obs <- ""
          },
          error = function(e) {
            error <- paste(e$message, e$call, collapse = ",")
            logger$error("caught error",
                         error = error,
                         source = source,
                         filename = source.filename)
            assign("error", error, envir = parent.env(environment()))
            status <- "exception"
            obs <- scraped.polyhedron$getErrors()
          }
        )
        self$ledger$updateStatus(
          source = source,
          source.filename = source.filename,
          status.field = "status.test",
          status = status,
          obs = obs
        )
        if (status == "testing") {
          tryCatch(
            {
              res <- testthat::expect_true(self$existsPolyhedron(
                source = source,
                polyhedron.name = polyhedron.name
              ))
              db.polyhedron <- self$getPolyhedron(
                source = source,
                polyhedron.name = polyhedron.name
              )
              scraped.polyhedron$state$inferEdges()
              testthat::expect_equal(scraped.polyhedron, db.polyhedron)
              status <- "tested"
            },
            error = function(e) {
              error <- paste(e$message, e$call, collapse = ",")
              logger$error("caught error",
                           error = error,
                           source = source,
                           polyhedron.name = polyhedron.name)
              assign("error", error, envir = parent.env(environment()))
              status <- "failed"
              obs <- scraped.polyhedron$getErrors()
            }
          )
          self$ledger$updateStatus(
            source = source,
            source.filename = source.filename,
            status.field = "status.test",
            status = status, obs = obs
          )
        }
        scraped.polyhedron
      }
      ret <- self$cover(
        mode = "test",
        sources = sources,
        covering.code = test.function,
        max.quant = max.quant
      )
      ret
    },
    #' @description
    #' generate Test tasks for selected polyhedra
    #' @param sources sources names
    #' @param polyhedra.names polyhedra names to cover (optional)
    #' @param TestTaskClass an R6 TestTaskClass class
    #' @param max.quant maximum numbers of polyhedra to cover
    #' @return A list with resulting TestTasks generated
    generateTestTasks = function(sources = names(self$sources.config),
                                 polyhedra.names = NULL,
                                 TestTaskClass,
                                 max.quant = 0) {
      logger <- getLogger(self)
      seed <- getPackageVersion()
      seed <- as.numeric(gsub("v|\\.", "", seed))
      seed <- seed * 121
      self$configPolyhedraRDSPath()
      if (file.exists(self$polyhedra.rds.file)) {
        polyhedra.db.saved <- readRDS(self$polyhedra.rds.file)
        if (!isCompatiblePolyhedraRDS(polyhedra.db.saved)) {
          stop("Incompatible polyhedra.db saved. Contact package maintainer.")
        }
      } else {
        # if database doesn't exists setup test as false
        logger$error(paste(
          "There is no polyhedra database so test ",
          "could not be ran"
        ))
        test <- FALSE
      }
      test.task.gen.function <- function(polyhedra.dir, source.config,
                                         polyhedron.file.id, source.filename) {
        scraped.polyhedron <- NULL
        source <- source.config$getName()
        polyhedron.ledger <- polyhedra.db.saved$ledger$
          df[polyhedra.db.saved$ledger$getIdFilename(source, source.filename), ]
        polyhedron.name <- polyhedron.ledger$scraped.name

        task <- TestTaskClass$new(
          polyhedra.db = polyhedra.db.saved,
          source.config = source.config,
          polyhedron.name = polyhedron.name,
          polyhedra.dir = polyhedra.dir,
          polyhedron.file.id = polyhedron.file.id,
          source.filename = source.filename
        )
        task
      }
      ret <- self$cover(
        mode = "test",
        sources = sources,
        covering.code = test.task.gen.function,
        polyhedra.names = polyhedra.names,
        max.quant = max.quant,
        seed = seed
      )
      ret
    },
    #' @description
    #' Schedules polyhedra sources for scraping
    #' @param sources.config sources configurations for scraping files
    #' @param source.filenames filenames of the polyhedron source definition
    #' @param max.quant maximum filenames to process
    #' @param save.on.change saves Database state after operation
    #' @return Modified `PolyhedraDatabase` object.
    schedulePolyhedraSources = function(sources.config =
                                          getPackageEnvir(".available.sources"),
                                        source.filenames = NULL,
                                        max.quant = 0,
                                        save.on.change = FALSE) {
      for (source in names(sources.config)) {
        self$configPolyhedraSource(
          source.config = sources.config[[source]],
          source.filenames = source.filenames,
          max.quant = max.quant,
          save.on.change = save.on.change
        )
      }
      self
    },
    #' @description
    #' Returns available sources in current database
    #' @return A vector with names of available sources
    getAvailableSources = function() {
      # TODO in ledger
      self$ledger$getAvailableSources()
    },
    #' @description
    #' Retrieves all polyhedron within the source those names match with search.string
    #' @param sources sources names
    #' @param search.string string for matching polyhedron names
    #' @param ignore.case ignore case in search string
    #' @return A list with resulting objects covered
    getAvailablePolyhedra = function(sources = self$getAvailableSources(),
                                     search.string = NULL, ignore.case = TRUE) {
      self$ledger$getAvailablePolyhedra(
        sources = sources,
        search.string = search.string,
        ignore.case = ignore.case
      )
    }
  )
)
