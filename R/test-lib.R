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
#' @noRd
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
#' @noRd
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
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @noRd
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


#' getPercentilPolyhedraQuant
#'
#' Returns polyhedra quantity of parameter percentil
#' @param percentil is the percentil which must be applied
#'        to estimate the figure
#' @param quant.min minimum quantity of files to return
#' @noRd
getPercentilPolyhedraQuant <- function(percentil, quant.min = 100){
  max(round(percentil * nrow(getAvailablePolyhedra())), quant.min)
}
