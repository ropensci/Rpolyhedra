#' Get a polyhedra object
#'
#' Return the polyhedra database handler.
#' @seealso PolyhedraDatabase.class
#'
#' @return .polyhedra
#' @export
getPolyhedraObject <- function() {
  getUserEnvir(".polyhedra")
}

#' Scrape polyedra objects
#'
#' Gets polyhedra objects from text files of
#' different sources, scheduling and scraping using
#' predefined configurations.
#'
#' @param scrape.config predefined configuration for scraping
#' @param source.filenames if not null specify which source filenames to scrape
#' @param sources.config the sources that will be used by the function
#' @return polyhedra db object
scrapePolyhedra <- function(scrape.config,
                source.filenames = NULL,
                sources.config = getPackageEnvir(".available.sources")) {
  scrapePolyhedraSources(max.quant.config.schedule =
               scrape.config[["max.quant.config.schedule"]],
             max.quant.scrape = scrape.config[["max.quant.scrape"]],
             time2scrape.source = scrape.config[["time2scrape.source"]],
             sources.config = sources.config,
             source.filenames = source.filenames,
             retry.scrape = scrape.config[["retry.scrape"]])

}

#' Scrape polyhedra sources
#'
#' Scrapes polyhedra objects from text files of
#' different sources, in order to make them available to the
#' package.
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
                        save.on.change = TRUE)
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
                              save.on.change = TRUE,
                              skip.still.queued = TRUE
                              )
  getPolyhedraObject()
}


#' Get available sources
#'
#' Gets the list of names of available sources in database to be used later as
#' references to the package.
#'
#' @seealso getAvailablePolyhedra, getPolyhedron
#'
#' @return sources string vector, which can be obtained from getAvailableSources()
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
#'cubes
getAvailableSources <- function(){
  getPolyhedraObject()$getAvailableSources()
}

#' Get available polyhedra
#'
#' Gets the list of names of available polyhedra and its status in
#' the polyhedra database, which can be later called with getPolyhedron
#'
#' @seealso getAvailableSources
#' @importFrom futile.logger flog.info
#' @param sources A string vector containing the source, which can be obtained from getAvailableSources().
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
#'cube
getAvailablePolyhedra <- function(sources =
                      names(getPackageEnvir(".available.sources")),
                      search.string = NULL){
  getPolyhedraObject()$getAvailablePolyhedra(sources = sources,
                      search.string = search.string)
}

#' Get polyhedron
#'
#' Gets a polyhedron from the database. It returns an R6 Class
#' with all its characteristics and functions.
#' The object returned, of type Polyhedron.class, allows to the
#' user to get access to all the functionality provided.
#'
#' @seealso getAvailablePolyhedra, getAvailableSources
#' @param source string vector, which can be obtained from getAvailableSources()
#' @param polyhedron.name  a valid name of a polyhedron in
#'        the database. Current names can be found with getAvailablePolyhedra()
#' @importFrom futile.logger flog.info
#' @export
#' @return polyhedron R6 object
#' @export
#' @examples
#' tetrahedron <- getPolyhedron(source = 'netlib',
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

#' Switch to full database
#'
#' Prompts user for changing database to fulldb in
#' user filespace. Also, allows the user to switch back to
#' the package database, which is a minimal one for testing purposes.
#'
#' @param env The environment to run on, can be PACKAGE,
#' HOME or NA. If NA, it asks the user for a an Environment.
#' @usage
#'     switchToFullDatabase(env=NA)
#' @return .data.env
#' @export
switchToFullDatabase <- function(env = NA){
  retVal <- selectDataEnv(env = env)
  if (retVal == "NOT_AVAILABLE") {
    futile.logger::flog.error("Full Database not available yet.")
  }
  retVal
}
