
#' polyhedronToJSON()
#'
#' Gets an JSON representation out of the polyhedron object
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @return an JSON document, ready to be saved as a String File
# TODO examples
#' @examples
#' #get the representation of a cube (netlib library)
#' library(Rpolyhedra)
#' polyhedronToJSON(getPolyhedron("netlib", "cube")$state)
#'
#' @import RJSONIO
polyhedronToJSON <- function(polyhedron.state.defined)
{

  stop("Not implemented")
}

#' persistPolyhedron
#'
#' persists a polyhedron to the defined path in a Zipped json format
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @param file.path
#' @return the path or None
# TODO examples
#' @examples
#' #get the representation of a cube (netlib library)
#' library(Rpolyhedra)
#' polyhedronToJSON(getPolyhedron("netlib", "cube")$state)
#'
persistPolyhedron <- function(polyhedron.state.defined, file.path)
{
  polyhedron.json <- polyhedronToJSON(polyhedron.state.defined)
  temp.file <- file(tempfile(pattern = "polyhedron", tmpdir = tempdir(), fileext = ""))
  write(polyhedron.json)

  zip(file.path, temp.file)
  unlink(temp.file)
}

#' hydratePolyhedron
#'
#' hydrates a polyhedron from the defined path
#'
#' @param file.path
#' @return polyhedron.state.defined
#'
hydratePolyhedron <- function(file.path)
{

}


