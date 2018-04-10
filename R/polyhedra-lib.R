





#' Polyhedron State
#'
#' This abstract class provide the basis from which polyhedron state class derivate.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{addError(current.error)}}{Adds an error to the error string and log it as info}
#'   \item{\code{scrape()}}{Scrapes the polyhedra folder files}
#'   \item{\code{geSolid()}}{returns the object corresponding to the solid}
#'   \item{\code{buildRGL(size = 1, origin = c(0, 0, 0), normalize.size = TRUE)}}{creates a RGL representation of the object}
#'   \item{\code{exportToXML()}}{Gets an XML representation out of the polyhedron object}
#' }
#' @field errors Errors string
#' @field source polyhedron definition source
#' @field number polyhedron number
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import rgl
#' @importFrom R6 R6Class
PolyhedronState.class <- R6::R6Class("PolyhedronState", public = list(
source = NA, number = NA, errors = "",
initialize = function(source, number) {
    self$source <- source
    self$number <- number
    self
},
addError = function(current.error) {
    self$errors <- paste(self$errors, current.error)
    futile.logger::flog.error(current.error)
    self$errors
},
scrape = function() {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
},
getSolid = function() {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
},
checkEdgesConsistency=function(){
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
},
buildRGL = function(size = 1, origin = c(0, 0, 0), normalize.size = TRUE) {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
},
exportToXML = function(){
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
}
))

#' Polyhedron State Netlib Scraper
#'
#' Scrapes polyhedra from a PHD file format.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(number, netlib.p3.lines)}}{Initializes the object, taking the number and PDH file as parameters}
#'   \item{\code{extract_fows_from_label(label.number, expected.label)}}{Extracts data from the label, taking the label number and the
#'   expected label as parameters}
#'   \item{\code{getLabels()}}{Gets the label from the polyhedron}
#'   \item{\code{scrapeNet(net.txt, offset = 0) }}{Scrape the net model}
#'   \item{\code{extractCFOutBrackets()}}{Gets the CF Out Brackets}
#'   \item{\code{scrapeVertices(vertices.txt)}}{Scrapes the vertices}
#'   \item{\code{setupLabelsOrder()}}{Sets up the order of labels included in PHD file}
#'   \item{\code{getDataFromLabel(label)}}{Gets data from the Label}
#'   \item{\code{scrape()}}{Scrapes the data from the PHD file}
#'   \item{\code{buildRGL(size = 1, origin = c(0, 0, 0), normalize.size = TRUE)}}{Builds the \code{RGL} mmodel}
#' }
#' @field netlib.p3.lines The path to the PHD files
#' @field labels.rows Labels - row of appearance
#' @field labels.map Labels - Map of content
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import stringr futile.logger
#' @importFrom R6 R6Class
PolyhedronStateNetlibScraper.class <- R6::R6Class("PolyhedronStateNetlibScraper",
inherit = PolyhedronState.class,
public = list(netlib.p3.lines = NA, labels.rows = NA, labels.map = NA, errors = "",
initialize = function(number, netlib.p3.lines) {
    super$initialize(source= "netlib",number = number)
    self$netlib.p3.lines <- netlib.p3.lines
    self$labels.map <- list()
    self
}, extract_fows_from_label = function(label.number, expected.label) {
    observer.label <- self$netlib.p3.lines[self$labels.rows[label.number]]
    observer.label <- sub("\\:", "", observer.label)
    if (observer.label != expected.label) {
        current.error <- paste(self$errors, "In label#", label.number, "Expected label was",
        expected.label, " and observed was", observer.label)
        self$addError(current.error)
    }
    first.data.row <- self$labels.rows[label.number]
    last.data.row <- self$labels.rows[label.number + 1]
    ret <- 0
    if (first.data.row > last.data.row) {
        self$addError(paste("for label", expected.label, "no valid rows: (fr, lr)",
        first.data.row, last.data.row))
    } else ret <- c((first.data.row + 1):(last.data.row - 1))
    ret
}, getLabels = function() {
    self$netlib.p3.lines[self$labels.rows]
}, scrapeNet = function(net.txt, offset = 0) {
    first.line <- strsplit(net.txt[1], split = " ")[[1]]
    faces <- as.numeric(first.line[1])
    max.degree <- as.numeric(first.line[2])
    if (faces != length(net.txt) - 1) self$addError(paste("declared ", faces,
    "faces, but having", length(faces) - 1))
    net <- list()
    cont <- 1
    for (f in c(2:length(net.txt))) {
        cf <- strsplit(net.txt[f], " ")[[1]]
        net[[cont]] <- as.numeric(cf[2:length(cf)]) + offset
        cont <- cont + 1
    }
    net
}, extractCFOutBrackets = function(x) {
    open.bracket.pos <- which(strsplit(x, "")[[1]] == "[")
    ret <- x
    if (length(open.bracket.pos) > 0) {
        ret <- substr(x, 1, open.bracket.pos - 1)
    }
    ret
}, scrapeVertices = function(vertices.txt) {
    first.line <- strsplit(vertices.txt[1], split = " ")[[1]]
    vertices.count <- as.numeric(first.line[1])
    max.degree <- as.numeric(first.line[2])
    if (vertices.count != length(vertices.txt) - 1) self$addError(paste("declared ",
    vertices.count, "vertices.count, but having", length(vertices.count) -
    1, elements))
    vertices <- data.frame(Pos3D_1 = numeric(), Pos3D_2 = numeric(), Pos3D_3 = numeric(),
    Pos3D_1_exp = numeric(), Pos3D_2_exp = numeric(), Pos3D_3_exp = numeric(),
    Pos3D_1_exp_text = numeric(), Pos3D_2_exp_text = numeric(), Pos3D_3_exp_text = numeric(),
    stringsAsFactors = FALSE)
    cont <- 1
    n.vertices <- length(vertices.txt)
    for (f in c(2:n.vertices)) {
        cf <- strsplit(vertices.txt[f], " ")[[1]]
        cf.outbrackets <- as.numeric(sapply(cf, FUN = function(x) self$extractCFOutBrackets(x)))
        cf.inbrackets <- stringr::str_extract(cf, "\\[([:graph:]*)\\]")
        cf.inbrackets <- sub("\\[", "", cf.inbrackets)
        cf.inbrackets <- sub("\\]", "", cf.inbrackets)
        futile.logger::flog.debug(paste("parsing vertex ", f, "/", n.vertices,
        " ", paste(cf.outbrackets, collapse = ","), " ", paste(cf.inbrackets,
        collapse = ","), sep = ""))
        vertices[cont, "Pos3D_1"] <- cf.outbrackets[1]
        vertices[cont, "Pos3D_2"] <- cf.outbrackets[2]
        vertices[cont, "Pos3D_3"] <- cf.outbrackets[3]
        vertices[cont, "Pos3D_1_exp"] <- eval(parse(text = cf.inbrackets[1]))
        vertices[cont, "Pos3D_2_exp"] <- eval(parse(text = cf.inbrackets[2]))
        vertices[cont, "Pos3D_3_exp"] <- eval(parse(text = cf.inbrackets[3]))
        vertices[cont, "Pos3D_1_exp_text"] <- cf.inbrackets[1]
        vertices[cont, "Pos3D_2_exp_text"] <- cf.inbrackets[2]
        vertices[cont, "Pos3D_3_exp_text"] <- cf.inbrackets[3]
        cont <- cont + 1
    }
    vertices
}, setupLabelsOrder = function() {
    for (r in c(1:length(self$labels.rows))) {
        p3.line <- self$labels.rows[r]
        current.label <- self$netlib.p3.lines[p3.line]
        current.label <- sub(":", "", current.label, fixed = TRUE)
        if (current.label %in% self$labels.map[[current.label]]) stop(paste(gettext("rpoly.row_for_label",
        domain = "R-Rpolyhedra"), current.label, gettext("rpoly.already_defined",
        domain = "R-Rpolyhedra")))
        self$labels.map[[current.label]] <- r
        futile.logger::flog.debug(paste("Assign order", r, "row", self$labels.rows[r],
        "to", current.label))
    }
    futile.logger::flog.debug(paste(names(self$labels.map), lapply(self$labels.map,
    FUN = function(x) self$netlib.p3.lines[self$labels.rows[x]]), collapse = "|",
    sep = "=>"))
    self$labels.map
}, getDataFromLabel = function(label) {
    r <- self$labels.map[[label]]
    ret <- NULL
    if (!is.null(r)) ret <- self$netlib.p3.lines[self$extract_fows_from_label(r,
    label)]
    ret
}, scrape = function() {
    # first check labels
    self$labels.rows <- grep("\\:", self$netlib.p3.lines)
    if (nchar(self$errors) > 0) {
        stop(paste(gettext("rpoly.scraping_issue", domain = "R-Rpolyhedra"),
        self$errors))
    }
    self$setupLabelsOrder()
    name <- self$getDataFromLabel("name")
    number <- self$getDataFromLabel("number")
    futile.logger::flog.debug(paste("Scraping polyhedron", number, name))
    symbol <- self$getDataFromLabel("symbol")
    dual <- self$getDataFromLabel("dual")
    sfaces <- self$getDataFromLabel("sfaces")
    svertices <- self$getDataFromLabel("svertices")
    hinges.txt <- self$getDataFromLabel("hinges")
    dih.txt <- self$getDataFromLabel("dih")
    vertices.txt <- self$getDataFromLabel("vertices")
    net.txt <- self$getDataFromLabel("net")
    net <- self$scrapeNet(net.txt, 1)
    solid.txt <- self$getDataFromLabel("solid")
    if (!is.null(solid.txt)) {
        solid <- self$scrapeNet(solid.txt, 1)
    } else {
        solid <- NULL
    }
    #TODO
    hinges <- NULL
    #TODO
    dih <- NULL
    vertices <- self$scrapeVertices(vertices.txt)
    ret <- PolyhedronStateDefined.class$new(source = self$source, number = number, name = name, symbol = symbol,
    dual = dual, sfaces = sfaces,
    svertices = svertices, net = net, solid = solid, hinges = hinges, dih = dih, vertices =vertices)
    ret
}, buildRGL = function(size = 1, origin = c(0, 0, 0), normalize.size = TRUE) {
    stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
},
exportToXML = function(){
    stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
}
))


#' Polyhedron State Dmccoey Scraper
#'
#' Scrapes polyhedra from a dmccooey file format
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(number, netlib.p3.lines)}}{Initializes the object, taking the number and PDH file as parameters}
#'   \item{\code{scrape()}}{Scrapes data from dmccooey file format}
#'   \item{\code{scrapeValues(values.lines)}}{Scrapes values}
#'   \item{\code{scrapeVertices(vertices.lines)}}{Scrapes vertices}
#'   \item{\code{scrapeFaces(face.lines)}}{Scrapes faces}
#'   \item{\code{buildRGL(size = 1, origin = c(0, 0, 0), normalize.size = TRUE)}}{Builds the \code{RGL} implementation}
#' }
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import stringr futile.logger
#' @importFrom R6 R6Class
PolyhedronStateDmccoeyScraper.class <- R6::R6Class("PolyhedronStateDmccoeyScraper",
  inherit = PolyhedronState.class,
  public = list(
    polyhedra.dmccoey.lines = NA,
    labels.map = NA,
    #state
    values = NA,
    vertices = NA,
    vertices.replaced = NA,
    faces = NA,
    #regexp
    regexp.values.names = NA,
    regexp.rn  = NA,
    regexp.values       = NA,
    regexp.vertex       = NA,
    regexp.faces        = NA,
  initialize = function(number, polyhedra.dmccoey.lines){
    super$initialize(source = "dmccooney", number)
    self$polyhedra.dmccoey.lines <- polyhedra.dmccoey.lines
    self$labels.map <- list()
    #init regexp

    self$regexp.values.names <- "C[0-9]+"
    self$regexp.rn  <- "[0-9]+\\.[0-9]+"
    self$regexp.values <- paste("(",self$regexp.values.names,")\\=(",self$regexp.rn,
    ")(\\=([[:graph:]]+))?",sep="")
    #V1=(C0,0.0,-1.0)
    self$regexp.vertex <- paste("(V[0-9]+)","\\=\\((,?-?(",
    self$regexp.values.names,"|",self$regexp.rn,")){3}\\)",sep="")
    self$regexp.faces  <- paste("\\{(,?[0-9]+)+}")
    self
  },
  scrapeValues = function(values.lines){
      self$values <- list()
      for (value in values.lines){
          value.name <- sub(self$regexp.values,"\\1",value)
          value.number <- sub(self$regexp.values,"\\2",value)
          self$values[[value.name]] <- value.number
      }
      self
  },
  scrapeVertices = function(vertices.lines){
      #TODO fancy regexp
      regexp.vertex.def <- "\\(([[:alpha:][0-9],.-]+)\\)"
      regexp.code.sign  <- "^\\-"
      self$vertices <- data.frame(Pos3D_1=character(), Pos3D_2=character(), Pos3D_3=character(),stringsAsFactors = FALSE)
      self$vertices.replaced <- data.frame(Pos3D_1=numeric(), Pos3D_2=numeric(), Pos3D_3=numeric(),stringsAsFactors = FALSE)

      for (vertex.line in vertices.lines){
          vertex.name <- sub(self$regexp.vertex,"\\1",vertex.line)
          vertex.row <- as.numeric(sub("^V","",vertex.name))+1
          vertex.def <- str_extract(vertex.line,regexp.vertex.def)
          vertex.coords <- strsplit(vertex.def,split=",")[[1]]
          vertex.coords <- gsub("\\(|\\)","",vertex.coords)
          self$vertices[vertex.row,] <- vertex.coords
          vertex.coords.replaced <- NULL
          for (d in 1:length(vertex.coords)){
              value.code <- vertex.coords[d]
              if (length(grep(regexp.code.sign,value.code))>0){
                  parity <- -1
                  value.code <- sub(regexp.code.sign,"",value.code)
              }
              else{
                  parity <- 1
              }

              is.value.numeric <- TRUE
              if (length(grep(self$regexp.values.names,value.code))>0){
                  is.value.numeric <- FALSE
              }
              if (!is.value.numeric){
                  if (value.code %in% names(self$values)){
                      value <- self$values[[value.code]]
                  }
                  else{
                      stop(paste("code",value.code,"not found in value definitions. Available values:",
                      paste(names(self$values),collapse=",")))
                  }
              }
              else{
                  value <- value.code
              }


              vertex.coords.replaced[d] <- parity*as.numeric(value)
          }
          self$vertices.replaced[vertex.row,] <- vertex.coords.replaced
      }
      self
  },
  scrapeFaces = function(face.lines){
      face.cont <- 1
      self$faces <- list()
      for (face.line in face.lines){
          face.def  <- gsub("\\{|\\}","",face.line)
          face.vertices <- strsplit(face.def,split=",")[[1]]
          self$faces[[as.character(face.cont)]] <- as.numeric(face.vertices)+1
          face.cont <- face.cont+1
      }
  },
  scrape = function() {
      #preprocess
      lines.num <- length(self$polyhedra.dmccoey.lines)
      self$polyhedra.dmccoey.lines[2:lines.num] <- gsub("[[:space:]]","",self$polyhedra.dmccoey.lines[2:lines.num])

      name <- self$polyhedra.dmccoey.lines[1]
      futile.logger::flog.debug(paste("Scraping dmccoey polyhedron", self$number,name))

      #values

      self$labels.map[["values"]] <- grep(self$regexp.values,self$polyhedra.dmccoey.lines)
      values.lines <- self$polyhedra.dmccoey.lines[self$labels.map[["values"]]]
      self$scrapeValues(values.lines)

      #vertex
      self$labels.map[["vertices"]]<-grep(self$regexp.vertex,self$polyhedra.dmccoey.lines)

      self$scrapeVertices(vertices.lines = self$polyhedra.dmccoey.lines[self$labels.map[["vertices"]]])

      #faces
      grep(self$regexp.faces,self$polyhedra.dmccoey.lines)
      self$labels.map[["faces"]]<-grep(self$regexp.faces,self$polyhedra.dmccoey.lines)
      self$scrapeFaces(face.lines = self$polyhedra.dmccoey.lines[self$labels.map[["faces"]]])



      ret <- PolyhedronStateDefined.class$new(source = self$source, number   = self$number,
      name = name,
      vertices = self$vertices.replaced,
      solid    = self$faces)
      ret
  }, buildRGL = function(size = 1, origin = c(0, 0, 0), normalize.size = TRUE) {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
  },
  exportToXML = function(){
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
  })
)


#' norm calculates norm of a vector
#' @param vector numeric vector
#'
norm <- function(vector){
    sqrt(sum(vector*vector))
}

#' Polyhedron State Defined
#'
#' Polyhedron state inside database.
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(source, number,name, symbol, dual,
#'               sfaces, svertices, net, solid, hinges, dih, vertices)}}{Initializes the object, taking defaults.}
#'   \item{\code{scrape()}}{Do nothing as the object is defined}
#'   \item{\code{getNet()}}{Gets the 2d net model}
#'   \item{\code{getSolid()}}{Gets the solid representation}
#'   \item{\code{triangulate(force = FALSE)}}{Generates the triangular faces model for generating tmesh }
#'   \item{\code{getBoundingBox(vertices.3d)}}{Gets the bounding box of the object}
#'   \item{\code{calculateMassCenter(size = 1, vertices.3d)}}{Calculates the object's Mass Center for parameter
#'         vertices}
#'   \item{\code{getNormalizedSize()}}{Normalizes the volume of the object to a tetrahedron bounding box}#'
#'   \item{\code{getTransformedVertices(size,origin)}}{Returns the vertices adjusted to size and origin parameters}#'
#'   \item{\code{buildRGL(size = 1, origin = c(0, 0, 0), normalize.size = TRUE)}}{
#'   Builds the RGL model, taking the object's size, the origin}
#'   \item{\code{exportToXML()}}{Gets an XML representation out of the polyhedron object}
#' }
#' @field source polyhedron definition source
#' @field number polyhedron arbitrary numeration (netlib|dmccooey)
#' @field name polyhedron name       (netlib|dmccooey)
#' @field symbol the eqn(1) input for two symbols separated by a tab; the Johnson symbol, and the Schlafli symbol (netlib)
#' @field dual  the name of the dual polyhedron optionally followed by a horizontal tab and the number of the dual (netlib)
#' @field sfaces polyhedron solid face list (netlib)
#' @field svertices polyhedron solid vertice list (netlib)
#' @field net polyhedron 2D net model with vertices defined for a planar representation (netlib)
#' @field hinges Polyhedron hinge list (netlib)
#' @field solid polyhedron list of edges which generate a solid (netlib|dmccooey)
#' @field dih Dih attribute (netlib)
#' @field vertices Polyhedron vertices list (netlib|dmccooey)
#' @field mass.center polyhedron mass center
#' @field transformation.matrix transformation matrix for calculations and visualizing polyhedron
# private
#' @field edges.cont  Edges count
#' @field edges.check Edges check degree property
#' @field vertices.centered centered vertices for applying transformation matrices
#' @field vertices.rgl Polyhedron triangulated vertices list for RGL
#' @field solid.triangulated Polyhedron solid (triangulated) for RGL visualization

#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import rgl testthat XML
#' @importFrom R6 R6Class
PolyhedronStateDefined.class <- R6::R6Class("PolyhedronStateDefined",
  inherit = PolyhedronState.class,
  private = list(mass.center = NULL,
                  #infered state
                  edges.cont = 0,
                  edges.check = NULL,
                  #rgl aux members
                  vertices.centered = NULL,
                  vertices.rgl = NULL,
                  solid.triangulated = NULL
                  ),
  public = list(name = NA, symbol = NA, dual = NA, sfaces = NA, svertices = NA,
                net = NA, hinges = NA, solid = NA, dih = NA, vertices = NA, edges = NULL,
                transformation.matrix = NA,
  initialize = function(source, number,name, vertices, solid, symbol=NULL, dual=NULL,
                        sfaces=NULL, svertices = NULL, net = NULL,  hinges = NULL, dih = NULL) {
    super$initialize(source = source, number = number)
    self$name <- name
    self$vertices <- vertices
    self$solid <- solid
    self$symbol <- symbol
    self$dual <- dual
    self$sfaces <- sfaces
    self$svertices <- svertices
    self$net <- net
    self$hinges <- hinges
    self$dih <- dih
    self$transformation.matrix <- identityMatrix()
    self$adjustVertices()
    self
  }, scrape = function() {
      self
  }, getSymbol = function() {
      self$symbol
  },
  adjustVertices = function(){
    vertices.3d <- sort(unique(unlist(self$solid)))
    private$vertices.centered <- self$vertices
    mass.center <- self$calculateMassCenter(vertices.3d = vertices.3d)
    sapply(vertices.3d,FUN=function(x){private$vertices.centered[x,1:3] <- private$vertices.centered[x,1:3] - mass.center})
    #private$vertices.centered[vertices.3d,1:3] <- private$vertices.centered[vertices.3d,1:3] - mass.center
    private$mass.center <- self$calculateMassCenter(vertices.3d = vertices.3d)

    #debug
    self.debug <<- self
    private.debug <<- self$getPrivate()
    self
  },
  getVertices = function(solid = FALSE) {
      ret<- self$vertices
      if (solid){
        vertices.in.faces <- NULL
        for (f in self$solid){
          for (v in f){
            vertices.in.faces <- union(vertices.in.faces,v)
          }
        }
        ret <- self$vertices[vertices.in.faces,]
      }
      ret
  }, getNet = function() {
      self$net
  }, getSolid = function() {
      self$solid
  },
  inferEdges = function(force.recalculation = FALSE){
      if (is.null(private$edges.check) | force.recalculation){
          private$edges.check <- data.frame(origin=numeric(),dest=numeric(),count=numeric())
          self$edges <- list()
          private$edges.cont <- 0
          for (f.number in 1:length(self$solid)){
              f <- self$solid[[f.number]]
              degree.f <- length(f)
              v.ant <- f[degree.f]
              for (it.v in c(1:length(f))){
                  v <- f[it.v]
                  if (v>v.ant){
                      v1 <- v.ant
                      v2 <- v
                  }
                  else{
                      v1 <- v
                      v2 <- v.ant
                  }
                  row.edge <- which(private$edges.check$origin==v1&
                  private$edges.check$dest==v2)
                  if (length(row.edge)==0){
                      row.edge <- nrow(private$edges.check)+1
                      count    <- 1
                      private$edges.cont <- private$edges.cont +1
                      self$edges[[as.character(private$edges.cont)]]<-c(v1,v2)
                  }
                  else{
                      count <- private$edges.check[row.edge,"count"]+1
                  }
                  private$edges.check[row.edge,]<- c(v1,v2,count)
                  v.ant <- v
              }
          }
      }
      self
  },
  checkEdgesConsistency=function(){
      ret <- NULL
      if (!is.null(self$solid)){
          self$inferEdges()
          rows.error <- which(private$edges.check$count!=2)
          if (length(rows.error)>0){
              error.edges <- paste(apply(private$edges.check[rows.error,],MARGIN=1,
              FUN=function(x)paste(names(x),x,sep="=>",collapse=",")),collapse="|")
              self$addError(paste("For", self$source, self$name,"faces definition is wrong as there are edges with count diff to 2:",error.edges))
          }
          ret <- private$edges.check[rows.error,]
      }
      ret
  },
  triangulate = function(force = FALSE) {
      if (is.null(private$solid.triangulated) | force) {
          net <- self$solid
          private$vertices.rgl <- private$vertices.centered[, 1:3]
          private$vertices.rgl$desc <- "solid"
          faces.size <- unlist(lapply(net, FUN = length))
          max.faces <- max(faces.size)
          # if (max.faces > 3) stop(paste('Not yet implemented for faces with', max.faces))
          f <- 1
          ret <- list()
          for (face in net) {
              current.vertex <- nrow(private$vertices.rgl)
              # the description considers vector referencing starting in 0.
              if (length(face) < 3) {
                  stop(paste("Problem with definition. Face defined with", length(face),
                  "vertex"))
              }
              if (length(face) == 3) {
                  tmesh <- face
              } else {
                  if (length(face) == 4) {
                      tmesh <- c(face[1], face[2], face[3], face[3], face[4], face[1])
                  }
                  if (length(face) >= 5) {
                      extra.mid.vertex <- apply(self$vertices[face, 1:3], MARGIN = 2,
                      FUN = mean)
                      extra.vertex.id <- current.vertex + 1
                      private$vertices.rgl[extra.vertex.id, 1:3] <- extra.mid.vertex
                      private$vertices.rgl[extra.vertex.id, 4] <- paste("extra-f", f,
                      sep = "")
                      last.v <- length(face)
                      tmesh <- NULL
                      for (v in 1:length(face)) {
                          tmesh <- c(tmesh, face[last.v], face[v], extra.vertex.id)
                          last.v <- v
                      }
                  }
              }
              ret[[f]] <- tmesh
              futile.logger::flog.debug(paste("triangulated f", f, length(face),
              "original", paste(face, collapse = ","), "triangulated", paste(tmesh,
              collapse = ",")))
              f <- f + 1
          }
          private$solid.triangulated <- ret
      }
      private$solid.triangulated
  }, getBoundingBox = function(vertices.def) {
      vertices.def.min <- apply(vertices.def, MARGIN = 2, FUN = min)
      vertices.def.max <- apply(vertices.def, MARGIN = 2, FUN = max)
      rbind(vertices.def.min, vertices.def.max)
  },
  checkInsideBoundingBox = function(vertices.df,v, bounding.box){
      inside <-rep(NA,3)
      pos3d <- vertices.df[v,]
      for (d in 1:3){
          inside[d] <- pos3d[d]>=bounding.box[1,d]&
          pos3d[d]<=bounding.box[2,d]
      }
      ret <- min(inside)==1
      if (ret){
          private$vertices.built <- union(private$vertices.built,v)
      }
      ret
  },
  calculateMassCenter = function(vertices.3d) {
    transformed.vertex <- private$vertices.centered[vertices.3d, c(1:3)]
    #transform3d(asHomogeneous(as.matrix(private$vertices[vertices.3d, c(1:3)])),transformation.matrix)

    # delaunayn(transformed.vertex)
    apply(transformed.vertex, MARGIN = 2, FUN = mean)
  },
  getNormalizedSize=function(size){
      vertices.def <- private$vertices.rgl[sort(unique(unlist(private$solid.triangulated))), c(1:3)]
      bounding.box <- self$getBoundingBox(vertices.def = vertices.def)
      volume <- prod(apply(bounding.box, MARGIN = 2,
      FUN = function(x) {x[2] - x[1]}))
      # 0.7501087 is tetrahedron bounding box
      size <- size * (0.7501087 / volume) ^ (1 / 3)
      size
  },
  #TODO remove
  getTransformedVertices.old = function(size = 1, origin = c(0,0,0)){
      positioned.vertices <- private$vertices.rgl[, c(1:3)] * size
      for (d in 1:3) {
          positioned.vertices[, d] <- positioned.vertices[, d] + origin[d] -
                                      private$mass.center[d]
      }
      positioned.vertices
  },
  getTransformedVertices = function(transformation.matrix){
    #positioned.vertices <- self$transformation.matrix private$vertices.rgl[, c(1:3)] %*%
    positioned.vertices <-  transform3d(asHomogeneous(as.matrix(private$vertices.rgl[, c(1:3)])),
                                        transformation.matrix)
    positioned.vertices <- positioned.vertices[,1:3]
    positioned.vertices
  },
  buildRGL = function(transformation.matrix = NULL, normalize.size = TRUE) {
      if (is.null(transformation.matrix)){
        transformation.matrix <- self$transformation.matrix
      }
      ret <- NULL
      self$inferEdges()
      if (length(self$solid) > 1) {
          triangulated.solid <- self$triangulate()
          if (normalize.size) {
              #TODO calculate size with transformation matrix
              size <- 1
              size <- self$getNormalizedSize(size)
          }
          positioned.vertices     <- self$getTransformedVertices(transformation.matrix)
          vertices <- as.matrix(cbind(positioned.vertices, 1))
          ret <- rgl::tmesh3d(c(t(vertices)), unlist(triangulated.solid))
      } else {
          futile.logger::flog.info(paste("For", self$name, " solid definition not found"))
          self$addError("solid definition not found")
      }
      ret
    },
    exportToXML = function()
    {
        polyhedronToXML(self)
    },
    getPrivate = function(){
      private
    }
))

#' Polyhedron
#'
#' Polyhedron container class, which is accesible by the final users upon call to \code{getPolyhedron()}
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(number, state = NULL)}}{Initializes the object}
#'   \item{\code{scrapeNetlib(polyhedron.lines)}}{{Scrapes polyhedra from the definition}}
#'   \item{\code{getName()}}{Gets the name from polyhedron definition}
#'   \item{\code{getState()}}{Gets the state from polyhedron definition}
#'   \item{\code{getSolid()}}{Gets the solid definition of polyhedron definition}
#'   \item{\code{isChecked()}}{Returns TRUE is polyhedron is checked}
#'   \item{\code{getErrors()}}{Returns errors collected in checking process}
#'   \item{\code{getRGLModel(size = 1, origin = c(0, 0, 0)}}{Builds the RGL model}
#'   \item{\code{exportToXML()}}{Gets an XML representation out of the polyhedron object}
#'   \item{\code{checkProperties(expected.vertices, expected.faces)}}{check polyhedron basic properties}
#'

#' }
#' @field number Polyhedron number
#' @field state Polyhedron state
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import rgl futile.logger testthat
#' @importFrom R6 R6Class
Polyhedron.class <- R6::R6Class("Polyhedron", public = list(number = NA, state = NA,
initialize = function(number, state = NULL) {
    self$number <- number
    if (!is.null(state)) {
        self$state <- state
    }
    self
},
scrapeNetlib = function(netlib.p3.lines) {
    self$state <- PolyhedronStateNetlibScraper.class$new(self$number, netlib.p3.lines)
    self$state <- self$state$scrape()
    self
},
scrapeDmccoey = function(polyhedra.dmccoey.lines) {
    self$state <- PolyhedronStateDmccoeyScraper.class$new(self$number, polyhedra.dmccoey.lines)
    self$state <- self$state$scrape()
    #Postprocess in defined state
    self
},
getName = function() {
    self$state$name
},
getState = function() {
    self$state
},
getSolid = function() {
    self$state$getSolid()
},
isChecked = function(){
    inconsistent.edges <- self$state$checkEdgesConsistency()
    ret <- FALSE
    if (!is.null(inconsistent.edges)){
        ret <- nrow(inconsistent.edges)==0
    }
    ret
},
getRGLModel = function(transformation.matrix=NULL) {
    futile.logger::flog.debug(paste("drawing", self$getName()),"model")
    self$state$buildRGL(transformation.matrix = transformation.matrix)
},
exportToXML = function(){
    self$state$exportToXML()
},
getErrors = function(){
    self$state$errors
},
checkProperties = function(expected.vertices, expected.faces){
    faces <- self$getSolid()
    expect_equal(length(faces),expected.faces)
    vertices.solid <- which(row.names(self$state$vertices) %in% unlist(faces))
    expect_equal(length(vertices.solid),expected.vertices)

    #check Edges consistency
    self$state$checkEdgesConsistency()
    self
}))



