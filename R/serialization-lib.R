
#' polyhedronToXML()
#'
#' Gets an XML representation out of the polyhedron object
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @return a XML document, ready to be saved as a String File
# TODO examples
#' @examples
#' #get the representation of a cube (netlib library)
#' library(Rpolyhedra)
#' polyhedronToXML(getPolyhedron("netlib", "cube")$getState())
#'
#' @import XML
#' @export
polyhedronToXML <- function(polyhedron.state.defined)
{
  xml <- XML::xmlTree()
  #xml$addTag("polyhedron", close=FALSE)
  #xml$closeTag()
  # TODO generate XML
  xml
}

#' persistPolyhedron
#'
#' persists a polyhedron to the defined path in a Zipped ascii RDS format
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @param file.path the path of the file to be persisted.
#' @return the path or None
#'
persistPolyhedron <- function(polyhedron.state.defined, file.path)
{
  polyhedron.xml <- polyhedronToXML(polyhedron.state.defined)
  temp.file <- file(tempfile(pattern = "polyhedron", tmpdir = tempdir(), fileext = ""))
  write(polyhedron.xml)

  zip(file.path, temp.file)
  unlink(temp.file)
}

#' hydratePolyhedron
#'
#' hydrates a polyhedron from the defined path
#'
#' @param file.path the path of the file to be hydrated.
#' @return polyhedron.state.defined
#'
hydratePolyhedron <- function(file.path)
{

}


