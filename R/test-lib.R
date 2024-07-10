#' Polyhedron test task
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
#' @docType class
#' @importFrom R6 R6Class
#' @noRd
PolyhedronTestTask <- R6::R6Class("PolyhedronTestTask",
  public = list(
    polyhedra.db = NA,
    source.config = NA,
    polyhedron.name = NA,
    initialize = function(polyhedra.db, source.config, polyhedron.name) {
      self$polyhedra.db <- polyhedra.db
      self$source.config <- source.config
      self$polyhedron.name <- polyhedron.name
      self
    },
    run = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    }
  )
)

#' Polyhedron test task scrape
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
#' @docType class
#' @import lgr
#' @importFrom R6 R6Class
#' @noRd
PolyhedronTestTaskScrape <- R6::R6Class("PolyhedronTestTaskScrape",
  inherit = PolyhedronTestTask,
  public = list(
    polyhedra.dir = NA,
    polyhedron.file.id = NA,
    source.filename = NA,
    # state
    scraped.polyhedron = NA,
    #' @field logger class logger
    logger = NA,
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.file.id,
                          source.filename) {
      super$initialize(
        polyhedra.db = polyhedra.db,
        source.config = source.config,
        polyhedron.name = polyhedron.name
      )
      self$polyhedra.dir <- polyhedra.dir
      self$polyhedron.file.id <- polyhedron.file.id
      self$source.filename <- source.filename
      self$logger <- genLogger(self)
      self
    },
    run = function() {
      logger <- getLogger(self)
      error <- ""
      scraped.name <- NA
      source <- self$source.config$getName()
      tryCatch(
        {
          obs <- ""
          self$scraped.polyhedron <- self$source.config$scrape(
            polyhedron.file.id = self$polyhedron.file.id,
            source.filename = file.path(self$polyhedra.dir, self$source.filename)
          )
          scraped.name <- self$scraped.polyhedron$getName()
          status <- "testing"
        },
        error = function(e) {
          error <- paste(e$message, collapse = ",")
          logger$error(paste("caught error", error))
          assign("error", error, envir = parent.env(environment()))
          status <- "exception"
          if (!is.na(self$scraped.polyhedron)) {
            obs <- scraped.polyhedron$getErrors()
          }
        }
      )

      expected.polyhedron <-
        self$polyhedra.db$getPolyhedron(
          source = source,
          polyhedron.name = scraped.name
        )

      expected.polyhedron.state <- expected.polyhedron$getState()
      expected.polyhedron.state$expectEqual(self$scraped.polyhedron)
    }
  )
)

#' Polyhedron test task edges consistency
#'
#' A Test task for running edges consistency test for current polyhedron
#'
#' @docType class
#' @noRd
PolyhedronTestTaskEdgesConsistency <- R6::R6Class(
  "PolyhedronTestTaskEdgesConsistency",
  inherit = PolyhedronTestTask,
  public = list(
    initialize = function(polyhedra.db, source.config, polyhedron.name,
                          polyhedra.dir, polyhedron.file.id, source.filename) {
      super$initialize(
        polyhedra.db = polyhedra.db, source.config =
          source.config,
        polyhedron.name = polyhedron.name
      )
      self
    },
    run = function() {
      source <- self$source.config$getName()
      current.polyhedron <-
        self$polyhedra.db$getPolyhedron(
          source = source,
          polyhedron.name = self$polyhedron.name
        )
      edges.inconsistent <- current.polyhedron$state$checkEdgesConsistency()
      testthat::expect_equal(nrow(edges.inconsistent), 0)
    }
  )
)


#' Get percentile polyhedra quantity
#'
#' Returns polyhedra quantity of parameter percentile
#' @param percentile is the percentile which must be applied
#'        to estimate the figure
#' @param quant.min minimum quantity of files to return
#' @noRd
getPercentilPolyhedraQuant <- function(percentile, quant.min = 100) {
  max(round(percentile * nrow(getAvailablePolyhedra())), quant.min)
}
