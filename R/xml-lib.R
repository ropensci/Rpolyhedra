
#' polyhedronToXML()
#'
#' Gets an XML representation out of the polyhedron object
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @return an XML document, ready to be converted to String with XML::saveXML()
# TODO examples
# @examples
# #get the representation of a cube (netlib library)
# library(Rpolyhedra)
# XML::saveXML(polyhedronToXML(getPolyhedron("netlib", "cube")$state))
#'
#' @import XML
#' @export
polyhedronToXML <- function(polyhedron.state.defined)
{
  stop("Not implemented")
}



