#' convertDFToXML()
#'
#' Allows to the creation of xml excerpts out of a data.frame.
#'
#' @param df the dataframe to convert
#' @param name the tag for the new section
#' @param node the node that will be acting as parent.
#' @return the node with the dataframe in it
#' @import XML
convertDFToXML <- function(df, name, node) {
  # Iterate over all rows
  lapply(1:nrow(df),
         function(rowi) {
           r <- XML::newXMLNode(name, parent=node)   # Create row tag
           for(var in names(df)) {   # Iterate over variables
             XML::newXMLNode(var, df[rowi, var], parent = r)
           }
         })
  node
}

#' validatePolyhedronXML()
#'
#' Validates the xml against the schema.
#'
#' @param polyhedron.xml the xml document object to test against the schema
#' @return the document or an error status.
#' @import XML
validatePolyhedronXML <- function (polyhedron.xml) {
  xsd <- XML::xmlSchemaParse(system.file("extdata", "polyhedron.xsd", package = "Rpolyhedra"))
  result <- XML::xmlSchemaValidate(xsd, polyhedron.xml)
  result
}



#' polyhedronToXML()
#'
#' Gets an XML representation out of the polyhedron object
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @return an XML document, ready to be converted to String with XML::saveXML()
#' @examples
#' #get the representation of a cube (netlib library)
#' library(Rpolyhedra)
#' XML::saveXML(polyhedronToXML(getPolyhedron("netlib", "cube")$state))
#'
#' @import XML
#' @export
polyhedronToXML <- function(polyhedron.state.defined)
{
  # Start empty XML document tree
  doc <- XML::newXMLDoc()

  #TODO: Apply tranformation matrix
  positioned.vertices <- polyhedron.state.defined$state$vertices
  faces <- polyhedron.state.defined$state$vertices
  edges <- polyhedron.state.defined$state$vertices
  # Start by adding a document tag at the root of the XML file
  root <- XML::newXMLNode("polyhedron", doc=doc, attrs=c(name=polyhedron.state.defined$name, dual=polyhedron.state.defined$dual))
  vertices <- XML::newXMLNode("vertices", doc = doc, parent = root, attrs=c(name=polyhedron.state.defined$name))
  vertices <- convertDFToXML(df = polyhedron.state.defined$vertices, name= "vertice", node = vertices)

  #TODO validate XML
  # validation.result <- validatePolyhedronXML(doc)
  # if (validation.result == 0){
  #   return(doc)
  # }else{
  #   return(validation.result)
  # }
  doc
}



