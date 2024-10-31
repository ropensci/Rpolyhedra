#' Get preloaded data filename
#'
#' Gets the path of Polyhedra preloaded data CSV file
#'
#' @param polyhedra.preloaded.data filename of polyhedra preloaded data csv
#' @return the path to the Polyhedra database file
#' @noRd
getPreloadedDataFilename <- function(polyhedra.preloaded.data =
                                       "polyhedra.preloaded.data.csv") {
  file.path(getDataDir(), polyhedra.preloaded.data)
}

#' Update polyhedra database
#'
#' Function for initializing database
#'
#' @param source.filenames if not null specify which source filenames to scrape
#' @return polyhedra db object
#' @noRd
updatePolyhedraDatabase <- function(source.filenames = NULL) {
  .polyhedra <- NULL
  polyhedra.rds.file <- getPolyhedraRDSPath()

  if (file.exists(polyhedra.rds.file)) {
    polyhedra.candidate <- readRDS(polyhedra.rds.file)
    if (isCompatiblePolyhedraRDS(polyhedra.candidate, halts = TRUE)) {
      .polyhedra <- polyhedra.candidate
    }
  }
  if (is.null(.polyhedra)) {
    .polyhedra <- PolyhedraDatabase$new()
  }

  setUserEnvir(".polyhedra", value = .polyhedra)

  .available.sources <- getUserEnvir(".available.sources")
  .available.scrapping.conf <- getUserEnvir(".available.scrapping.conf")

  # "dev-tetrahedron" "dev-minimal" "pkg-minimal" "fulldb"
  setUserEnvir(".scrape.config", "fulldb")
  # When release version, change parameter to "pkg-minimal"
  .scrape.config <- getUserEnvir(".scrape.config")
  current.config <- .available.scrapping.conf[[.scrape.config]]
  if (is.null(current.config)) {
    stop(paste("Configuration", .scrape.config, " does not exists"))
  }
  scrapePolyhedra(
    scrape.config = .available.scrapping.conf[[.scrape.config]],
    source.filenames = source.filenames,
    sources.config = .available.sources
  )
}

#' Download RPolyhedra supporting files
#'
#' Downloads the files from the remote location
#' @param logger logger for inheriting threshold from calling class/function
#' @return TRUE if successful, FALSE otherwise
#' @importFrom utils unzip
#' @importFrom utils download.file
#' @importFrom utils zip
#' @import lgr
#' @noRd
downloadRPolyhedraSupportingFiles <- function(logger = lgr) {
  retVal <- "SUCCESS"

  if (checkDatabaseVersion() == "UPDATE") {
    if (getDataEnv() == "HOME") {
      db.version <- getPackageDB()
      db.url <- paste(
        "https://github.com/qbotics/RpolyhedraDB/archive/refs/tags/v",
        db.version,
        ".zip",
        sep = ""
      )
      td <- file.path(tempdir(), "Rpolyhedra")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      unlink(dir(td, full.names = TRUE))
      zipFile <- tempfile(tmpdir = td, fileext = ".zip")
      # download file to tempfile
      oldw <- getOption("warn")
      options(warn = -1)
      retVal <- tryCatch(
        {
          utils::download.file(db.url, destfile = zipFile, mode = "wb")
          "SUCCESS"
        },
        error = function(e) {
          "NOT_AVAILABLE"
        }
      )
      options(warn = oldw)
      if (retVal == "SUCCESS") {
        logger$debug(
          "Downloaded file to",
          zipFile = zipFile
        )
        tdb <- file.path(td, "db")
        dir.create(tdb, recursive = TRUE, showWarnings = FALSE)
        utils::unzip(zipfile = zipFile, exdir = tdb)
        logger$debug(
          "Decompressed",
          dest.folder = tdb
        )
        db.package.path <- dir(tdb, full.names = TRUE)
        tmp.db.path <- list.files(path = db.package.path)[1]
        files.to.copy <- list.files(db.package.path)
        # copy files
        file.copy(
          from = file.path(db.package.path, files.to.copy),
          to = getUserSpace(), recursive = TRUE
        )
        # delete tmp path
        unlink(file.path(td, tmp.db.path), recursive = TRUE)
      } else {
        logger$warn(
          "url not found",
          url = db.url
        )
      }
    }
  }
  retVal
}

#' Copy files to external data
#'
#' Copies files from/to the home directory to the package one to build a preconfigured package.
#'
#' @param source.folder folder of polyhedra data sources
#' @param dest.folder   folder of polyhedra data destination
#' @param force indicate if existings directories must be overwritten
#' @param logger logger for inheriting threshold from calling class/function
#' @return TRUE if successful
#' @import lgr
#' @noRd
copyFilesToExtData <- function(source.folder = getDataDir(data.env = "HOME"),
                               dest.folder = getDataDir(data.env = "PACKAGE"),
                               force = FALSE, logger = lgr) {
  polyhedra.ledger <- getPolyhedraObject()$
    ledger$getAvailablePolyhedra(ret.fields = NULL)
  polyhedra.ledger.scraped <-
    polyhedra.ledger[polyhedra.ledger$status == "scraped", ]

  dir.create(dest.folder, showWarnings = FALSE, recursive = TRUE)
  # check existing sources
  existing <- FALSE
  .available.sources <- getUserEnvir(".available.sources")
  for (source in names(.available.sources)) {
    source.config <- .available.sources[[source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.exists(dest.folder.source)) {
      existing <- TRUE
    }
  }
  if (existing & !force) {
    stop(paste(
      "Cannot copy files: they exists in destination",
      dest.folder.source, " Call ",
      "the function with force=TRUE or remove them manually"
    ))
  }
  # clean dirs
  for (source in names(.available.sources)) {
    source.config <- .available.sources[[source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.exists(dest.folder.source)) {
      unlink(dest.folder.source, recursive = TRUE)
    }
    dir.create(dest.folder.source, showWarnings = FALSE, recursive = TRUE)
  }
  # copy files
  # copy version
  file.copy(file.path(source.folder, "version"), dest.folder,
    overwrite = TRUE
  )
  file.copy(file.path(source.folder, "polyhedra.preloaded.data.csv"),
    dest.folder,
    overwrite = TRUE
  )

  # copy polyhedra source files
  cont <- 0
  for (i in seq_len(nrow(polyhedra.ledger.scraped))) {
    current.polyhedron <- polyhedra.ledger.scraped[i, ]
    source.config <- .available.sources[[current.polyhedron$source]]
    dest.folder.source <- source.config$getBaseDir(dest.folder)
    if (file.copy(
      file.path(
        source.config$getBaseDir(source.folder),
        current.polyhedron$source.filename
      ),
      dest.folder.source
    )) {
      cont <- cont + 1
    }
  }
  logger$info(
    "Copied",
    cont = cont,
    dest.folder = dest.folder
  )
  # copy RDS
  file.copy(file.path(source.folder, "polyhedra.RDS"), dest.folder)
  TRUE
}


#' Polyhedron scraper configuration class
#'
#' Abstract class for configuring specific scrapers for Different sources
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{Initializes the object}
#' }
#' @import lgr
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfiguration <- R6::R6Class(
  "PolyhedronScraperConfiguration",
  public = list(
    #' @field name configuration name
    name = NA,
    #' @field base.dir configuration base.dir
    base.dir = NA,
    #' @field logger class logger
    logger = NA,
    initialize = function(name, base.dir) {
      self$name <- name
      self$base.dir <- base.dir
      self$logger <- genLogger(self)
      self
    },
    getName = function() {
      self$name
    },
    getBaseDir = function(home.dir.data) {
      file.path(home.dir.data, "sources", self$base.dir)
    },
    getPolyhedraFiles = function(home.dir.data) {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    }
  )
)


#' Polyhedron scraper configuration class (Netlib)
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
#' @docType class
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfigurationNetlib <- R6::R6Class(
  "PolyhedronScraperConfigurationNetlib",
  inherit = PolyhedronScraperConfiguration,
  public = list(
    initialize = function(name) {
      super$initialize(
        name = "netlib",
        base.dir = file.path("netlib.org", "polyhedra")
      )
    },
    getPolyhedraFiles = function(home.dir.data) {
      # wget -r -np -k https://netlib.org/polyhedra/ data/netlib.org
      polyhedra.dir <- self$getBaseDir(home.dir.data)
      polyhedra.files <- dir(polyhedra.dir)
      polyhedra.files <- polyhedra.files[grep("[0-9]+", polyhedra.files)]
      polyhedra.files <- polyhedra.files[order(as.numeric(polyhedra.files))]
      polyhedra.files
    },
    scrape = function(polyhedron.file.id, source.filename.path) {
      logger <- getLogger(self)
      polyhedra.netlib.lines <- readLines(source.filename.path)
      polyhedron.file.id <- source.filename.path
      polyhedron.file.id <- strsplit(polyhedron.file.id, split = "/")[[1]]
      polyhedron.file.id <- polyhedron.file.id[length(polyhedron.file.id)]
      current.polyhedron <- Polyhedron$new(file.id = polyhedron.file.id)
      current.polyhedron$scrapeNetlib(netlib.p3.lines = polyhedra.netlib.lines)
      logger$debug(
        "Parsed",
        source.filepath = source.filename.path,
        name = current.polyhedron$state$name
      )
      current.polyhedron
    }
  )
)

#' Polyhedron scraper configuration class (Dmccooey)
#'
#' Scraper configuration for Dmccooey source
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{getPolyhedraFiles(home.dir.data)}}{returns the file names on the netlib database}
#'   \item{\code{scrape(polyhedron.file.id, source.filename)}}{scrapes the object}
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#' @noRd
PolyhedronScraperConfigurationDmccooey <- R6::R6Class(
  "PolyhedronScraperConfigurationDmccooey",
  inherit = PolyhedronScraperConfiguration,
  public = list(
    initialize = function(name) {
      super$initialize(
        name = "dmccooey",
        base.dir = file.path("dmccooey.com", "polyhedra")
      )
      self
    },
    getPolyhedraFiles = function(home.dir.data) {
      # wget -r -np -k http://dmccooey.com/polyhedra/
      # for filename in `find dmccooey.com/polyhedra/*.html -type f`; do
      #   echo $filename
      #    wget "www."${filename::-5}".txt" -P dmccooey.com/polyhedra/
      # done

      polyhedra.dir <- self$getBaseDir(home.dir.data)
      polyhedra.files <- dir(polyhedra.dir)
      polyhedra.files <- polyhedra.files[grep("\\.txt", polyhedra.files)]
      polyhedra.files
    },
    scrape = function(polyhedron.file.id, source.filename.path) {
      logger <- getLogger(self)
      polyhedra.dmccooey.lines <- readLines(source.filename.path)
      current.polyhedron <- Polyhedron$new(file.id = polyhedron.file.id)
      current.polyhedron$scrapeDmccooey(
        polyhedra.dmccooey.lines =
          polyhedra.dmccooey.lines
      )
      logger$debug(
        "parsed",
        source.filename.path = source.filename.path,
        name = current.polyhedron$state$name
      )
      current.polyhedron
    }
  )
)

#' Check database version
#'
#' Determines if there is a need for a database update by checking the
#' version file of both the package and the database installation.
#'
#' @return "UPDATE" if an update is required, "NO_ACTION_REQUIRED"
#'      otherwise.
#' @noRd
checkDatabaseVersion <- function() {
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

#' Is compatible Polyhedra RDS file
#'
#' Tests if the polyhedra RDS is compatible with the current format
#'
#' @param .polyhedra.candidate polyhedra db to test
#' @param halts indicates whether it has to halt execution when it
#' is not compatible
#' @import lgr
#' @noRd
isCompatiblePolyhedraRDS <- function(.polyhedra.candidate =
                                       getPolyhedraObject(),
                                     halts = FALSE,
                                     logger = lgr) {
  file <- class(.polyhedra.candidate)
  compatible <- FALSE
  error <- ""

  if (file[[1]] == "PolyhedraDatabase") {
    db.version <- getPackageDB()
    compatible <- !is.null(db.version)
    if (compatible) {
      compatible <- .polyhedra.candidate$getVersion() == db.version
    } else {
      db.version <- "#not-defined#"
    }

    if (!compatible) {
      error <- paste("Incompatible! DB version observed = ",
        .polyhedra.candidate$getVersion(),
        " expected = ",
        db.version,
        ". Code version= ",
        getPackageVersion(),
        ".",
        sep = ""
      )
    }
  } else {
    error <- paste("Incompatible! PolyhedraDatabase class is ",
      file[[1]],
      ".",
      sep = ""
    )
  }
  if (nchar(error) > 0) {
    if (halts) {
      stop(paste(error, "Contact package maintainer."))
    } else {
      logger$error(error)
    }
  }
  compatible
}




#' Get package version
#'
#' Obtains code version from the Description
#' @importFrom utils packageVersion
#' @noRd
getPackageVersion <- function() {
  as.character(utils::packageVersion("Rpolyhedra"))
}

#' getPackageDB
#'
#' Obtains the database version from environment
#' @noRd
getPackageDB <- function() {
  .package.db <- getUserEnvir(".package.db")
  ret <- .package.db[[getPackageVersion()]]
  if (is.null(ret)) {
    ret <- getPackageVersion()
  }
  ret
}

#' Get database version
#'
#' Obtains the generation code version from the database version file
#' @noRd
getDatabaseVersion <- function() {
  version <- NULL
  version.file <- file.path(getDataDir(), "version")
  if (file.exists(version.file)) {
    version <- readLines(version.file, n = 1)
  }
  version
}
