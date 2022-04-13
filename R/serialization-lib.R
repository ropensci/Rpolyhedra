
#' Convert dataframe to XML
#'
#' Allows to the creation of xml excerpts out of a data.frame.
#'
#' @param df the dataframe to convert
#' @param name the tag for the new section
#' @param node the node that will be acting as parent.
#' @return the node with the dataframe in it
#' @importFrom XML newXMLNode
#' @noRd
convertDFToXML <- function(df, name, node) {
  # Iterate over all rows
  lapply(
    seq_len(nrow(df)),
    function(rowi) {
      # Create row tag
      r <- XML::newXMLNode(name, parent = node)
      # Iterate over variables
      for (var in names(df)) {
        if (!is.na(df[rowi, var])) {
          XML::newXMLNode(var, df[rowi, var], parent = r)
        }
      }
    }
  )
  node
}

#' Polyhedron to XML
#'
#' Gets an XML representation out of the polyhedron object
#'
#' @param polyhedron.state.defined the polyhedron to get a representation from
#' @param is.transformed.vertices  flag which states if vertices are in original position or transformationMatrix applied
#' @return an XML document, ready to be converted to String with XML::saveXML()
#' @examples
#' # get the representation of a cube (netlib library)
#' XML::saveXML(polyhedronToXML(getPolyhedron("netlib", "cube")$state))
#'
#' @importFrom XML newXMLDoc
#' @importFrom XML newXMLNode
#' @export
polyhedronToXML <- function(polyhedron.state.defined,
                            is.transformed.vertices = TRUE) {
  # Start empty XML document tree
  doc <- XML::newXMLDoc()

  if (is.transformed.vertices) {
    vertices <- polyhedron.state.defined$getTransformedVertices()
    applied.transformation.matrix <-
      polyhedron.state.defined$transformation.matrix
  } else {
    vertices <- polyhedron.state.defined$vertices[, 1:3]
    applied.transformation.matrix <- identityMatrix()
  }
  vertices <- as.data.frame(vertices)

  names(vertices) <- c("x", "y", "z")

  applied.transformation.matrix <- as.data.frame(applied.transformation.matrix)

  faces <- polyhedron.state.defined$solid
  # Start by adding a document tag at the root of the XML file
  rootNode <- XML::newXMLNode("polyhedron",
    doc = doc,
    attrs = c(
      name = polyhedron.state.defined$name,
      dual = polyhedron.state.defined$dual
    )
  )
  verticesNode <- XML::newXMLNode("vertices", doc = doc, parent = rootNode)
  verticesNode <- convertDFToXML(
    df = vertices, name = "vertex",
    node = verticesNode
  )
  facesNode <- XML::newXMLNode("faces", doc = doc, parent = rootNode)
  for (face.id in seq_len(length(faces))) {
    face <- faces[face.id]
    faceNode <- XML::newXMLNode("face",
      doc = doc, parent = facesNode,
      attrs = c(
        id = face.id, definition =
          paste(face[[1]], collapse = ",")
      )
    )
  }
  identityMatrixNode <- XML::newXMLNode("identityMatrix",
    doc = doc,
    parent = rootNode
  )
  identityMatrixNode <- convertDFToXML(
    df = applied.transformation.matrix,
    name = "row",
    node = identityMatrixNode
  )
  doc
}
