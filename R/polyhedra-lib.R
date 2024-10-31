#' PolyhedronState
#' @description
#' This abstract class provide the basis from which every polyhedron state class derivate.
#' @importFrom R6 R6Class
#' @import lgr
#' @docType class
#' @author ken4rab
PolyhedronState <- R6::R6Class("PolyhedronState",
  public = list(
    #' @field source polyhedron definition source
    source = NA,
    #' @field file.id polyhedron file id
    file.id = NA,
    #' @field errors Errors string
    errors = "",
    #' @field logger class logger
    logger = NA,
    #' @description
    #' Create a polyhedronState object
    #' @param source the source file
    #' @param file.id the file id
    #' @return A new  PolyhedronState object.
    initialize = function(source, file.id) {
      self$source <- source
      self$file.id <- file.id
      self$logger <- genLogger(self)
      self
    },
    #' '@description
    #' Adds an error to the error string and log it as info
    #' @param current.error the error to add
    addError = function(current.error) {
      logger <- getLogger(self)
      self$errors <- paste(self$errors, current.error)
      logger$error(current.error)
      self$errors
    },
    #' @description
    #' Scrapes the polyhedra folder files
    scrape = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Get Polyhedron name
    #' @return string with polyhedron name
    getName = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Returns the object corresponding to the solid
    getSolid = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Checks edge consistency
    checkEdgesConsistency = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Apply transformation matrix to polyhedron
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    applyTransformationMatrix = function(transformation.matrix) {
      stop("Abstract class")
    },
    #' @description
    #' Creates a 'rgl' representation of the object
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    buildRGL = function(transformation.matrix) {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Gets an XML representation out of the polyhedron object
    exportToXML = function() {
      stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
    }
  )
)

#' PolyhedronStateNetlibScraper
#' @description
#' Scrapes polyhedra from a PHD file format.
#'
#' @import lgr
#' @importFrom stringr str_extract
#' @importFrom R6 R6Class
#' @docType class
#' @author ken4rab
PolyhedronStateNetlibScraper <- R6::R6Class(
  "PolyhedronStateNetlibScraper",
  inherit = PolyhedronState,
  public = list(
    #' @field netlib.p3.lines The path to the PHD files
    netlib.p3.lines = NA,
    #' @field labels.rows Labels - row of appearance
    labels.rows = NA,
    #' @field labels.map Labels - Map of content
    labels.map = NA,
    #' @field errors the errors found
    errors = "",
    #' @description
    #' Initializes the object, taking the file.id and PDH file as parameters
    #' @param file.id the file id
    #' @param netlib.p3.lines the lines to add
    #' @return A new  PolyhedronStateNetlibScraper object.
    initialize = function(file.id, netlib.p3.lines) {
      super$initialize(source = "netlib", file.id = file.id)
      self$netlib.p3.lines <- netlib.p3.lines
      self$labels.map <- list()
      self
    },
    #' @description
    #' Extracts data from the label, taking the label number and the
    #'   expected label as parameters
    #' @param label.number the label number
    #' @param expected.label the expected label
    extractRowsFromLabel = function(label.number, expected.label) {
      observer.label <- self$netlib.p3.lines[self$labels.rows[label.number]]
      observer.label <- sub("\\:", "", observer.label)
      if (observer.label != expected.label) {
        current.error <- paste(
          self$errors, "In label#",
          label.number, "Expected label was",
          expected.label, " and observed was", observer.label
        )
        self$addError(current.error)
      }
      first.data.row <- self$labels.rows[label.number]
      last.data.row <- self$labels.rows[label.number + 1]
      ret <- 0
      if (first.data.row > last.data.row) {
        self$addError(paste(
          "for label", expected.label,
          "no valid rows: (fr, lr)",
          first.data.row, last.data.row
        ))
      } else {
        ret <- c((first.data.row + 1):(last.data.row - 1))
      }
      ret
    },
    #' @description
    #' get Labels from current netlib file description
    #' @return a list containing labels from netlib file description
    getLabels = function() {
      self$netlib.p3.lines[self$labels.rows]
    },
    #' @description
    #' scrape Net Model from netlib format
    #' @param net.txt a vector containing net model in netlib format
    #' @param offset in numbering vertices
    #'
    #' @return a list containing a net model
    scrapeNet = function(net.txt, offset = 0) {
      first.line <- strsplit(net.txt[1], split = " ")[[1]]
      faces <- as.numeric(first.line[1])
      max.degree <- as.numeric(first.line[2])
      if (faces != length(net.txt) - 1) {
        self$addError(paste(
          "declared ", faces,
          "faces, but having", length(faces) - 1
        ))
      }
      net <- list()
      cont <- 1
      for (f in seq_len(length(net.txt))) {
        if (f > 1) {
          # First line processed above
          cf <- strsplit(net.txt[f], " ")[[1]]
          net[[cont]] <- as.numeric(cf[2:length(cf)]) + offset
          cont <- cont + 1
        }
      }
      net
    },
    #' @description
    #' Remove brackets for current field content
    #' @param x a string containing brackets
    #' @return value
    extractCFOutBrackets = function(x) {
      open.bracket.pos <- which(strsplit(x, "")[[1]] == "[")
      ret <- x
      if (length(open.bracket.pos) > 0) {
        ret <- substr(x, 1, open.bracket.pos - 1)
      }
      ret
    },
    #' @description
    #' scrape vertices described in netlib format
    #' @param vertices.txt vector containing netlib format vertices
    #' @return data.frame containing netlib vertices
    scrapeVertices = function(vertices.txt) {
      logger <- getLogger(self)
      first.line <- strsplit(vertices.txt[1], split = " ")[[1]]
      vertices.count <- as.numeric(first.line[1])
      max.degree <- as.numeric(first.line[2])
      if (vertices.count != length(vertices.txt) - 1) {
        self$addError(paste(
          "declared ",
          vertices.count, "vertices.count, but having", length(vertices.count) -
            1, elements
        ))
      }
      vertices <- data.frame(
        Pos3D_1 = numeric(), Pos3D_2 = numeric(),
        Pos3D_3 = numeric(),
        Pos3D_1_exp = numeric(),
        Pos3D_2_exp = numeric(),
        Pos3D_3_exp = numeric(),
        Pos3D_1_exp_text = numeric(),
        Pos3D_2_exp_text = numeric(),
        Pos3D_3_exp_text = numeric(),
        stringsAsFactors = FALSE
      )
      cont <- 1
      n.vertices <- length(vertices.txt)
      for (v in seq_len(n.vertices)) {
        if (v > 1) {
          # First line processed above

          cf <- strsplit(vertices.txt[v], " ")[[1]]
          cf.outbrackets <- as.numeric(vapply(cf,
            FUN = function(x) {
              self$extractCFOutBrackets(x)
            },
            FUN.VALUE = character(1)
          ))
          cf.inbrackets <- stringr::str_extract(cf, "\\[([:graph:]*)\\]")
          cf.inbrackets <- sub("\\[", "", cf.inbrackets)
          cf.inbrackets <- sub("\\]", "", cf.inbrackets)
          logger$debug("parsing vertex ",
            v = v,
            total = n.vertices,
            cf.outbrackets =
              paste(cf.outbrackets,
                collapse = ","
              ),
            cf.inbrackets =
              paste(cf.inbrackets,
                collapse = ","
              )
          )
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
      }
      vertices
    },
    #' @description
    #' setupLabelsOrder
    #' @param vertices.txt vector containing netlib format vertices
    #' @return data.frame containing netlib vertices
    setupLabelsOrder = function() {
      logger <- getLogger(self)
      for (r in seq_len(length(self$labels.rows))) {
        p3.line <- self$labels.rows[r]
        current.label <- self$netlib.p3.lines[p3.line]
        current.label <- sub(":", "", current.label, fixed = TRUE)
        if (current.label %in% self$labels.map[[current.label]]) {
          stop(paste(
            gettext("rpoly.row_for_label",
              domain = "R-Rpolyhedra"
            ), current.label,
            gettext("rpoly.already_defined",
              domain = "R-Rpolyhedra"
            )
          ))
        }
        self$labels.map[[current.label]] <- r
        logger$debug(
          "Assign order",
          r = r,
          row = self$labels.rows[r],
          to = current.label
        )
      }
      logger$debug("Labels",
        map = names(self$labels.map),
        labels.map = paste(lapply(self$labels.map,
          FUN = function(x) {
            self$netlib.p3.lines[self$labels.rows[x]]
          }
        ),
        collapse = "|",
        sep = "=>"
        )
      )
      self$labels.map
    },
    #' @description
    #' Get data from label specified as parameter
    #' @param label the label to get data from
    #' @return value
    getDataFromLabel = function(label) {
      r <- self$labels.map[[label]]
      ret <- NULL
      if (!is.null(r)) {
        ret <- self$netlib.p3.lines[
          self$extractRowsFromLabel(r, label)
        ]
      }
      ret
    },
    #' @description
    #' get Polyhedron name
    #' @return string with polyhedron name
    getName = function() {
      self$getDataFromLabel("name")
    },
    #' @description
    #' scrape Netlib polyhedron definition
    #' @return A new PolyhedronStateDefined object.
    scrape = function() {
      logger <- getLogger(self)
      # first check labels
      self$labels.rows <- grep("\\:", self$netlib.p3.lines)
      if (nchar(self$errors) > 0) {
        stop(paste(
          gettext("rpoly.scraping_issue",
            domain = "R-Rpolyhedra"
          ),
          self$errors
        ))
      }
      self$setupLabelsOrder()
      name <- self$getDataFromLabel("name")
      file.id <- self$getDataFromLabel("number")
      logger$debug(
        "Scraping polyhedron",
        file.id = file.id,
        name = name
      )

      symbol <- self$getDataFromLabel("symbol")
      if (is.null(symbol)) {
        symbol <- ""
      }
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
      # TODO
      hinges <- NULL
      # TODO
      dih <- NULL
      vertices <- self$scrapeVertices(vertices.txt)


      ret <- PolyhedronStateDefined$new(
        source = self$source,
        file.id = file.id, name = name,
        symbol = symbol,
        dual = dual, sfaces = sfaces, svertices = svertices,
        vertices = vertices, net = net, solid = solid,
        hinges = hinges, dih = dih
      )
      ret
    },
    #' @description
    #' Apply transformation matrix to polyhedron
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    applyTransformationMatrix = function(transformation.matrix) {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Creates a 'rgl' representation of the object
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    buildRGL = function(transformation.matrix) {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' serializes object in XML
    exportToXML = function() {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    }
  )
)


#' PolyhedronStateDmccooeyScraper
#' @description
#' Scrapes polyhedra from a dmccooey file format
#'
#' @docType class
#' @import lgr
#' @importFrom R6 R6Class
#' @docType class
#' @author ken4rab
PolyhedronStateDmccooeyScraper <- R6::R6Class(
  "PolyhedronStateDmccooeyScraper",
  inherit = PolyhedronState,
  public = list(
    # regexp
    #' @field regexp.values.names regexp for scraping values names
    regexp.values.names = NA,
    #' @field regexp.rn regexp for scraping real numbers
    regexp.rn = NA,
    #' @field regexp.values regexp for scraping values
    regexp.values = NA,
    #' @field regexp.vertex regexp for scraping vertices
    regexp.vertex = NA,
    #' @field regexp.faces regexp for scraping faces
    regexp.faces = NA,
    #' @field polyhedra.dmccooey.lines dmccooey polyhedra definition lines
    polyhedra.dmccooey.lines = NA,
    #' @field labels.map labels map where values are
    labels.map = NA,
    # state
    #' @field values labels map where values are
    values = NA,
    #' @field vertices specification
    vertices = NA,
    #' @field vertices.replaced 3D values
    vertices.replaced = NA,
    #' @field faces definition
    faces = NA,
    #' @description
    #' Initialize Dmccooey scraper
    #' @param file.id identifier of the definition file.
    #' @param polyhedra.dmccooey.lines raw Dmccooey definition file lines
    #' @return A new  PolyhedronStateDmccooeyScraper object.
    initialize = function(file.id, polyhedra.dmccooey.lines) {
      super$initialize(source = "dmccooney", file.id)
      self$polyhedra.dmccooey.lines <- polyhedra.dmccooey.lines
      self$labels.map <- list()
      # init regexp
      self
    },
    #' @description
    #' setupRegexp for Dmccooey definition
    #' @return This PolyhedronStateDmccooeyScraper object with regexp defined.
    setupRegexp = function() {
      self$regexp.values.names <- "C[0-9]+"
      self$regexp.rn <- "[0-9]+\\.[0-9]+"
      self$regexp.values <- paste("(", self$regexp.values.names,
        ")\\=(",
        self$regexp.rn,
        ")(\\=([[:graph:]]+))?",
        sep = ""
      )
      # V1=(C0,0.0,-1.0)
      self$regexp.vertex <- paste("(V[0-9]+)",
        "\\=\\((,?-?(",
        self$regexp.values.names,
        "|",
        self$regexp.rn,
        ")){3}\\)",
        sep = ""
      )
      self$regexp.faces <- paste("\\{(,?[0-9]+)+}")
      self
    },
    #' @description
    #' scrape values from Dmccooey definition
    #' @param values.lines values definitions in Dmccooey source
    #' @return This PolyhedronStateDmccooeyScraper object with values defined.
    scrapeValues = function(values.lines) {
      self$values <- list()
      for (value in values.lines) {
        value.name <- sub(self$regexp.values, "\\1", value)
        value.number <- sub(self$regexp.values, "\\2", value)
        self$values[[value.name]] <- value.number
      }
      self
    },
    #' @description
    #' scrape polyhedron vertices from definition
    #' @param vertices.lines vertices definitions in Dmccooey source
    #' @return This PolyhedronStateDmccooeyScraper object with faces defined.
    scrapeVertices = function(vertices.lines) {
      # TODO fancy regexp
      regexp.vertex.def <- "\\(([[:alpha:][0-9],.-]+)\\)"
      regexp.code.sign <- "^\\-"
      self$vertices <- data.frame(
        Pos3D_1 = character(), Pos3D_2 = character(),
        Pos3D_3 = character(), stringsAsFactors = FALSE
      )
      self$vertices.replaced <- data.frame(
        Pos3D_1 = numeric(),
        Pos3D_2 = numeric(),
        Pos3D_3 = numeric(),
        stringsAsFactors = FALSE
      )

      for (vertex.line in vertices.lines) {
        vertex.name <- sub(self$regexp.vertex, "\\1", vertex.line)
        vertex.row <- as.numeric(sub("^V", "", vertex.name)) + 1
        vertex.def <- stringr::str_extract(vertex.line, regexp.vertex.def)
        vertex.coords <- strsplit(vertex.def, split = ",")[[1]]
        vertex.coords <- gsub("\\(|\\)", "", vertex.coords)
        self$vertices[vertex.row, ] <- vertex.coords
        vertex.coords.replaced <- NULL
        for (d in seq_len(length(vertex.coords))) {
          value.code <- vertex.coords[d]
          if (length(grep(regexp.code.sign, value.code)) > 0) {
            parity <- -1
            value.code <- sub(regexp.code.sign, "", value.code)
          } else {
            parity <- 1
          }

          is.value.numeric <- TRUE
          if (length(grep(self$regexp.values.names, value.code)) > 0) {
            is.value.numeric <- FALSE
          }
          if (!is.value.numeric) {
            if (value.code %in% names(self$values)) {
              value <- self$values[[value.code]]
            } else {
              stop(paste("code", value.code, paste(
                "not found in value",
                "definitions... Available values:",
                paste(names(self$values), collapse = ",")
              )))
            }
          } else {
            value <- value.code
          }
          vertex.coords.replaced[d] <- parity * as.numeric(value)
        }
        self$vertices.replaced[vertex.row, ] <- vertex.coords.replaced
      }
      self
    },
    #' @description
    #' scrape polyhedron faces from definition
    #' @param faces.lines face
    #' @return This PolyhedronStateDmccooeyScraper object with faces defined.
    scrapeFaces = function(faces.lines) {
      face.cont <- 1
      self$faces <- list()
      for (face.line in faces.lines) {
        face.def <- gsub("\\{|\\}", "", face.line)
        face.vertices <- strsplit(face.def, split = ",")[[1]]
        self$faces[[as.character(face.cont)]] <- as.numeric(face.vertices) + 1
        face.cont <- face.cont + 1
      }
      self
    },
    #' @description
    #' scrape Dmccooey polyhedron definition
    #' @return A new PolyhedronStateDefined object.
    scrape = function() {
      logger <- getLogger(self)
      stopifnot(!is.na(self$regexp.values))

      # preprocess
      lines.num <- length(self$polyhedra.dmccooey.lines)
      self$polyhedra.dmccooey.lines[2:lines.num] <- gsub(
        "[[:space:]]",
        "",
        self$polyhedra.dmccooey.lines[2:lines.num]
      )

      name <- self$polyhedra.dmccooey.lines[1]
      logger$debug(
        "Scraping dmccooey polyhedron",
        file.id = self$file.id,
        name = name
      )
      # values

      self$labels.map[["values"]] <- grep(
        self$regexp.values,
        self$polyhedra.dmccooey.lines
      )
      values.lines <- self$polyhedra.dmccooey.lines[self$labels.map[["values"]]]
      self$scrapeValues(values.lines)


      # vertex
      self$labels.map[["vertices"]] <- grep(
        self$regexp.vertex,
        self$polyhedra.dmccooey.lines
      )

      self$scrapeVertices(vertices.lines = self$polyhedra.dmccooey.lines[
        self$labels.map[["vertices"]]
      ])

      # faces
      grep(self$regexp.faces, self$polyhedra.dmccooey.lines)
      self$labels.map[["faces"]] <- grep(
        self$regexp.faces,
        self$polyhedra.dmccooey.lines
      )
      self$scrapeFaces(faces.lines = self$polyhedra.dmccooey.lines[
        self$labels.map[["faces"]]
      ])


      ret <- PolyhedronStateDefined$new(
        source = self$source,
        file.id = self$file.id,
        name = name,
        vertices = self$vertices.replaced,
        solid = self$faces
      )
      ret
    },
    #' @description
    #' get Polyhedron name
    #' @return string with polyhedron name
    getName = function() {
      self$polyhedra.dmccooey.lines[1]
    },
    #' @description
    #' Apply transformation matrix to polyhedron
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    applyTransformationMatrix = function(transformation.matrix) {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' Creates a 'rgl' representation of the object
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    buildRGL = function(transformation.matrix) {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    },
    #' @description
    #' serializes object in XML
    exportToXML = function() {
      stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    }
  )
)


#' norm
#'
#' @description
#' Calculates the norm of a vector
#'
#' @param vector numeric vector
#' @noRd
#'
norm <- function(vector) {
  sqrt(sum(vector * vector))
}

#' PolyhedronStateDefined
#' @description
#' Polyhedron State scraped and defined
#'
#'
#' @import lgr
#' @importFrom rgl identityMatrix
#' @importFrom rgl transform3d
#' @importFrom rgl asHomogeneous
#' @importFrom geometry convhulln
#' @importFrom R6 R6Class
#' @import jsonlite
#' @docType class
#' @author ken4rab
PolyhedronStateDefined <- R6::R6Class(
  "PolyhedronStateDefined",
  inherit = PolyhedronState,
  private = list(
    # polyhedron mass center
    mass.center = NA,
    # Edges count
    edges.cont = 0,
    # Edges check degree property
    edges.check = NULL,
    # vertices definition for solid 3d object
    vertices.id.3d = NULL,
    # Polyhedron triangulated vertices list for 'rgl'
    vertices.rgl = NULL,
    #  Polyhedron solid (triangulated)
    solid.triangulated = NULL
  ),
  public = list(
    #' @field file.id polyhedron filename in original
    file.id = NA,
    #' @field source polyhedron definition source (netlib|dmccooey)
    source = NA,
    #' @field name polyhedron name (netlib|dmccooey)
    name = NA,
    #' @field symbol the eqn(1) input for two symbols separated by a tab;
    #' the Johnson symbol, and the Schlafli symbol (netlib)
    symbol = NA,
    #' @field dual  the name of the dual polyhedron optionally followed
    #' by a horizontal tab and the number of the dual (netlib)
    dual = NA,
    #' @field sfaces polyhedron solid face list (netlib)
    sfaces = NA,
    #' @field svertices polyhedron solid vertices list (netlib)
    svertices = NA,
    #' @field vertices Polyhedron vertices list (netlib|dmccooey)
    vertices = NA,
    #' @field vertices.centered centered vertices for applying
    #' transformation matrices
    vertices.centered = NULL,
    #' @field net polyhedron 2D net model with vertices defined for
    #'  a planar representation (netlib)
    net = NA,
    #' @field solid polyhedron list of edges which generate a
    #' solid (netlib|dmccooey)
    solid = NA,
    #' @field hinges Polyhedron hinge list (netlib)
    hinges = NA,
    #' @field dih Dih attribute (netlib)
    dih = NA,
    #' @field edges polyhedron edges (netlib|dmccooey)
    edges = NULL,
    #' @field transformation.matrix transformation matrix for
    #'  calculations and visualizing polyhedron
    transformation.matrix = NA,
    #' @description
    #' object initialization routine
    #'
    #' @param source the library to use
    #' @param file.id identifier of the definition file.
    #' @param name the polyhedron name
    #' @param vertices the vertices
    #' @param solid the solid object
    #' @param net the net
    #' @param symbol the symbol
    #' @param dual whether it is dual or not
    #' @param sfaces the solid faces
    #' @param svertices the solid vertices
    #' @param hinges the hinges
    #' @param dih the dih
    #' @param normalize.size whether it has to normalize the size or not
    #' @return A new  PolyhedronStateDefined object.
    initialize = function(source, file.id, name,
                          vertices, solid, net = NULL,
                          symbol = "", dual = NULL, sfaces = NULL,
                          svertices = NULL, hinges = NULL, dih = NULL,
                          normalize.size = TRUE) {
      super$initialize(source = source, file.id = file.id)
      self$name <- name
      self$vertices <- vertices
      self$solid <- solid
      if (is.null(symbol)) {
        symbol <- ""
      }
      self$symbol <- symbol
      self$dual <- dual
      self$sfaces <- sfaces
      self$svertices <- svertices
      self$net <- net
      self$hinges <- hinges
      self$dih <- dih
      if (is.null(self$solid)) {
        self$addError(paste("Solid definition not found"))
      }
      self$transformation.matrix <- identityMatrix()
      if (nchar(self$errors) == 0) {
        self$adjustVertices(normalize.size = normalize.size)
      }
      self
    },
    #' @description
    #' scrape polyhedron.
    #' As the state is defined this functions do nothing
    #' @return current object
    scrape = function() {
      self
    },
    #' @description
    #' saves the state to a JSON file
    #' @param pretty  whether json output is pretty or not
    #' @return a json object
    saveToJson = function(pretty = FALSE) {
      ret <- list()
      vertices <- self$vertices[, paste("Pos3D", 1:3, sep = "_")]
      names(vertices) <- c("x", "y", "z")
      ret$vertices <- vertices
      ret$solid <- self$solid
      jsonlite::toJSON(ret, pretty = pretty)
    },
    #' @description
    #' get Polyhedron name
    #' @return string with polyhedron name
    getName = function() {
      self$name
    },
    #' @description
    #' get Polyhedron symbol
    #' @return string with polyhedron symbol
    getSymbol = function() {
      self$symbol
    },
    #' @description
    #' adjust polyhedron Vertices
    #'
    #' @param normalize.size whether it has to normalize the size or not
    #' @return modified  PolyhedronStateDefined object.
    adjustVertices = function(normalize.size = TRUE) {
      logger <- getLogger(self)
      private$vertices.id.3d <- sort(unique(unlist(self$solid)))
      vertices.id <- seq_len(nrow(self$vertices))
      if (!identical(private$vertices.id.3d, vertices.id))
      {
        solid.not.vertices <- setdiff(private$vertices.id.3d, vertices.id)
        vertices.not.solid <- setdiff(vertices.id, private$vertices.id.3d)
        logger$warn("Differences in solid than in vertices",
                    missing.solid = solid.not.vertices,
                    missing.vertices = vertices.not.solid)
      }
      private$vertices.id.3d <- intersect(private$vertices.id.3d, vertices.id)
      self$vertices.centered <- self$vertices
      #browser()
      mass.center <- self$calculateMassCenter(
        vertices.id.3d = private$vertices.id.3d,
        applyTransformation = FALSE
      )
      vapply(private$vertices.id.3d,
        FUN = function(x) {
          self$vertices.centered[x, 1:3] <-
            self$vertices.centered[x, 1:3] - mass.center
          TRUE
        },
        FUN.VALUE = logical(1)
      )
      private$mass.center <- self$calculateMassCenter(
        vertices.id.3d = private$vertices.id.3d,
        applyTransformation = FALSE
      )
      # normalize size
      if (normalize.size) {
        normalized.size <- self$getNormalizedSize(1)
        self$vertices.centered <- self$vertices.centered[, 1:3] * normalized.size
      }
      else{
        # If no size normalization, vertices.centered = vertices.id.3d
        vapply(private$vertices.id.3d,
               FUN = function(x) {
                 self$vertices.centered[x, 1:3] <-
                   self$vertices.centered[x, 1:3] + mass.center
                 TRUE
               },
               FUN.VALUE = logical(1)
        )

      }
      self
    },
    #' @description
    #' Get the polyhedron state
    #' @param solid toggles the production of solid vertices.
    getVertices = function(solid = FALSE) {
      ret <- self$vertices
      if (solid) {
        vertices.in.faces <- NULL
        for (f in self$solid) {
          for (v in f) {
            vertices.in.faces <- union(vertices.in.faces, v)
          }
        }
        ret <- self$vertices[vertices.in.faces, ]
      }
      ret
    },
    #' @description
    #' Gets the net property
    getNet = function() {
      self$net
    },
    #' @description
    #' Gets the solid property
    getSolid = function() {
      self$solid
    },
    #' @description
    #' Infer edges
    #' @param force.recalculation forces the recalculation of the edges
    inferEdges = function(force.recalculation = FALSE) {
      if (is.null(private$edges.check) | force.recalculation) {
        private$edges.check <- data.frame(
          origin = numeric(),
          dest = numeric(),
          count = numeric()
        )
        self$edges <- list()
        private$edges.cont <- 0
        for (f.number in seq_len(length(self$solid))) {
          f <- self$solid[[f.number]]
          degree.f <- length(f)
          v.ant <- f[degree.f]
          for (it.v in seq_len(length(f))) {
            v <- f[it.v]
            if (v > v.ant) {
              v1 <- v.ant
              v2 <- v
            } else {
              v1 <- v
              v2 <- v.ant
            }
            row.edge <- which(private$edges.check$origin == v1 &
              private$edges.check$dest == v2)
            if (length(row.edge) == 0) {
              row.edge <- nrow(private$edges.check) + 1
              count <- 1
              private$edges.cont <- private$edges.cont + 1
              self$edges[[as.character(private$edges.cont)]] <- c(v1, v2)
            } else {
              count <- private$edges.check[row.edge, "count"] + 1
            }
            private$edges.check[row.edge, ] <- c(v1, v2, count)
            v.ant <- v
          }
        }
      }
      self
    },
    #' @description
    #' Checks edges consistency
    checkEdgesConsistency = function() {
      ret <- NULL
      if (!is.null(self$solid)) {
        self$inferEdges()
        rows.error <- which(private$edges.check$count != 2)
        if (length(rows.error) > 0) {
          error.edges <- paste(apply(private$edges.check[rows.error, ],
            MARGIN = 1,
            FUN = function(x) {
              paste(names(x), x, sep = "=>", collapse = ",")
            }
          ),
          collapse = "|"
          )
          self$addError(current.error = paste(
            "For",
            self$source,
            self$name,
            paste(
              "faces definition is wrong as there",
              "are edges with count diff to 2:"
            ),
            error.edges
          ))
        }
        ret <- private$edges.check[rows.error, ]
      }
      ret
    },
    #' @description
    #' Triangulates the polyhedron
    #' @param force forces the triangulation.
    triangulate = function(force = FALSE) {
      logger <- getLogger(self)
      if (is.null(self$vertices.centered)) {
        stop(paste("vertices.centered must be called before triangulate"))
      }
      if (is.null(private$solid.triangulated) | force) {
        net <- self$solid
        private$vertices.rgl <- self$vertices.centered[, 1:3]
        private$vertices.rgl$desc <- "solid"
        faces.size <- unlist(lapply(net, FUN = length))
        max.faces <- max(faces.size)
        # if (max.faces > 3) stop(paste('Not yet
        # implemented for faces with', max.faces))
        f <- 1
        ret <- list()
        for (face in net) {
          current.vertex <- nrow(private$vertices.rgl)
          # the description considers vector referencing starting in 0.
          if (length(face) < 3) {
            stop(paste(
              gettext("rpoly.polyhedron_invalid_face",
                domain = "R-Rpolyhedra"
              ),
              length(face)
            ))
          }
          if (length(face) == 3) {
            tmesh <- face
          } else {
            if (length(face) == 4) {
              tmesh <- c(
                face[1], face[2], face[3],
                face[3], face[4], face[1]
              )
            }
            if (length(face) >= 5) {
              extra.mid.vertex <- apply(self$vertices.centered[
                face, 1:3
              ],
              MARGIN = 2,
              FUN = mean
              )
              extra.vertex.id <- current.vertex + 1
              private$vertices.rgl[extra.vertex.id, 1:3] <- extra.mid.vertex
              private$vertices.rgl[extra.vertex.id, 4] <- paste("extra-f", f,
                sep = ""
              )
              last.v <- length(face)
              tmesh <- NULL
              for (v in seq_len(length(face))) {
                tmesh <- c(tmesh, face[last.v], face[v], extra.vertex.id)
                last.v <- v
              }
            }
          }
          ret[[f]] <- tmesh
          logger$debug(
            "triangulated",
            f = f,
            length = length(face),
            original = paste(face, collapse = ","),
            triangulated = paste(tmesh,
              collapse = ","
            )
          )
          f <- f + 1
        }
        private$solid.triangulated <- ret
      }
      private$solid.triangulated
    },
    #' @description
    #' Gets the convex hull
    #'
    #' @param transformation.matrix the transformation matrix
    #' @param vertices.id.3d  the vertices ids
    #' @return the convex hull
    getConvHull = function(transformation.matrix = self$transformation.matrix,
                           vertices.id.3d = private$vertices.id.3d) {
      vertices.def <- self$getTransformedVertices(self$vertices.centered,
        transformation.matrix = transformation.matrix
      )
      vertices.def <- vertices.def[private$vertices.id.3d, ]
      convhulln <- convhulln(vertices.def, options = c("FA", "n"))
      convhulln
    },
    #' @description
    #' Calculates the center of mass.
    #' @param vertices.id.3d  the vertices ids
    #' @param applyTransformation does it need to apply transformations?
    calculateMassCenter = function(vertices.id.3d = private$vertices.id.3d,
                                   applyTransformation = TRUE) {
      transformed.vertex <- self$vertices[vertices.id.3d, c(1:3)]
      if (applyTransformation) {
        vertices.centered <- asHomogeneous(as.matrix(
          self$vertices[vertices.id.3d, c(1:3)]
        ))
        transformed.vertex <- transform3d(
          vertices.centered,
          self$transformation.matrix
        )
      }

      transformed.vertex <- transformed.vertex[, 1:3]
      apply(transformed.vertex, MARGIN = 2, FUN = mean)
    },
    #' @description
    #' Gets the normalized size
    #' @param size the object's size
    getNormalizedSize = function(size) {
      convex.hull <- self$getConvHull()
      volume <- convex.hull$vol
      # 0.1178511 is tetrahedron convex hull volume
      size <- size * (0.1178511 / volume)^(1 / 3)
      size
    },
    #' @description
    #' Gets the transformed vertices
    #' @param vertices input vertices
    #' @param transformation.matrix the transformation matrix
    getTransformedVertices = function(vertices = self$vertices.centered,
                                      transformation.matrix = self$transformation.matrix) {
      transformed.vertices <- transform3d(asHomogeneous(
        as.matrix(vertices[, c(1:3)])
      ),
      matrix = transformation.matrix
      )
      transformed.vertices <- transformed.vertices[, 1:3]
      transformed.vertices
    },
    #' @description
    #' Resets the transformation matrix
    resetTransformationMatrix = function() {
      self$transformation.matrix <- identityMatrix()
      self$transformation.matrix
    },
    #' @description
    #' Apply transformation matrix to polyhedron
    #' @param transformation.matrix the transformation matrix to apply to the polyhedron
    #' @return an applied transformation.matrix
    applyTransformationMatrix = function(transformation.matrix) {
      self$transformation.matrix <- transformation.matrix %*%
        self$transformation.matrix
      self$transformation.matrix
    },
    #' @description
    #' Build 'rgl'
    #' @param transformation.matrix the transformation matrix
    buildRGL = function(transformation.matrix = NULL) {
      logger <- getLogger(self)
      if (is.null(transformation.matrix)) {
        transformation.matrix <- self$transformation.matrix
      } else {
        transformation.matrix <- transformation.matrix %*%
          self$transformation.matrix
      }
      ret <- NULL
      self$inferEdges()
      if (length(self$solid) > 1) {
        triangulated.solid <- self$triangulate()
        transformed.vertices <- self$getTransformedVertices(
          vertices = private$vertices.rgl,
          transformation.matrix = transformation.matrix
        )
        transformed.vertices <- checkVertices(
          vertices = self$getVertices()[, 1:3],
          transformed.vertices = transformed.vertices,
          triangulated.solid
        )
        vertices <- as.matrix(cbind(transformed.vertices, 1))
        ret <- rgl::tmesh3d(c(t(vertices)), unlist(triangulated.solid))
      } else {
        logger$info(
          "Solid definition not found",
          name = self$name
        )
        self$addError("solid definition not found")
      }
      ret
    },
    #' @description
    #' Exports the object to XML format
    exportToXML = function() {
      polyhedronToXML(self)
    },
    #' @description
    #' Determines if a polyhedron is equal to this one.
    #' @param polyhedron the polyhedron to compare to.
    expectEqual = function(polyhedron) {
      compatible <- !is.null(polyhedron$state$serialize)
      if (compatible) {
        self.serialized <- self$serialize()
        polyhedron.serialized <- polyhedron$getState()$serialize()
        # check all same fields
        testthat::expect_equal(
          names(polyhedron.serialized),
          names(self.serialized)
        )
        # check values for all fields
        for (name in names(self.serialized)) {
          testthat::expect_equal(
            self.serialized[[name]],
            polyhedron.serialized[[name]]
          )
        }
      } else {
        stop(paste("Not compatible polyhedron", polyhedron$getName()))
      }
    },
    #' @description
    #' Serialize the object.
    serialize = function() {
      ret <- list()
      ret[["source"]] <- self$source
      ret[["name"]] <- self$name
      ret[["net"]] <- self$net
      ret[["file.id"]] <- self$file.id
      ret[["sfaces"]] <- self$sfaces
      ret[["vertices"]] <- self$vertices
      ret[["solid"]] <- self$solid
      ret[["svertices"]] <- self$svertices
      ret[["symbol"]] <- self$symbol
      ret[["dual"]] <- self$dual
      # Private:
      # edges.check: data.frame
      # edges.cont: 6
      # mass.center: -0.77777777691603 -0.833950387905475 -0.839177040579277
      # solid.triangulated: list
      # vertices.rgl: data.frame
      ret
    }
  )
)


#' PolyhedronStateDeserializer
#'
#' @description
#' Polyhedron state for deserialize from database
#'
#'
#' @importFrom R6 R6Class
#' @docType class
#' @author ken4rab
PolyhedronStateDeserializer <- R6::R6Class(
  "PolyhedronStateDeserializer",
  inherit = PolyhedronState,
  public = list(
    #' @field serialized.polyhedron polyhedron definition serialized
    serialized.polyhedron = NA,
    #' @description
    #' Initialize PolyhedronStateDeserializer object
    #' @param serialized.polyhedron a serialized polyhedron
    #' @return A new  PolyhedronStateDeserializer object.
    initialize = function(serialized.polyhedron) {
      self$serialized.polyhedron <- serialized.polyhedron
      self
    },
    #' @description
    #' Generates a PolyhedronStateDefined from a serialized polyhedron
    #' @return A new  PolyhedronStateDefined object.
    scrape = function() {
      sp <- self$serialized.polyhedron
      source <- sp$source
      file.id <- sp$file.id
      name <- sp$name
      symbol <- sp$symbol
      dual <- sp$dual
      sfaces <- sp$sfaces
      svertices <- sp$svertices
      net <- sp$net
      solid <- sp$solid
      hinges <- sp$hindges
      dih <- sp$dih
      vertices <- sp$vertices

      ret <- PolyhedronStateDefined$new(
        source = source,
        file.id = file.id,
        name = name,
        symbol = symbol,
        dual = dual,
        sfaces = sfaces,
        svertices = svertices,
        vertices = vertices,
        net = net,
        solid = solid,
        hinges = hinges,
        dih = dih
      )
      ret
    }
  )
)


#' Polyhedron
#'
#' @description
#' Polyhedron container class, which is accessible by the final users upon call
#'
#' @importFrom R6 R6Class
#' @docType class
#' @author ken4rab
Polyhedron <- R6::R6Class("Polyhedron",
  public = list(
    #' @field file.id Polyhedron file.id
    file.id = NA,
    #' @field state Polyhedron state
    state = NA,
    #' @field logger class logger
    logger = NA,
    #' @description
    #' Create a polyhedronState object
    #' @param state polyhedron state object
    #' @param file.id the file id
    #' @return A new  Polyhedron object.
    initialize = function(file.id, state = NULL) {
      self$file.id <- file.id
      if (!is.null(state)) {
        self$state <- state
      }
      self$logger <- genLogger(self)
      self
    },
    #' @description
    #' scrape Netlib polyhedron definition
    #' @param netlib.p3.lines vector with netlib definition lines
    #' @return A new  PolyhedronStateDefined object.
    scrapeNetlib = function(netlib.p3.lines) {
      self$state <- PolyhedronStateNetlibScraper$new(
        self$file.id, netlib.p3.lines
      )
      self$state <- self$state$scrape()
      self
    },
    #' @description
    #' scrape Dmccooey polyhedron definition
    #' @param polyhedra.dmccooey.lines vector with Dmccooey definition lines
    #' @return A new  PolyhedronStateDefined object.
    scrapeDmccooey = function(polyhedra.dmccooey.lines) {
      self$state <- PolyhedronStateDmccooeyScraper$new(
        file.id = self$file.id, polyhedra.dmccooey.lines = polyhedra.dmccooey.lines
      )
      self$state$setupRegexp()

      self$state <- self$state$scrape()
      # Postprocess in defined state
      self
    },
    #' @description
    #' deserialize a polyhedron state definition
    #' @param serialized.polyhedron a serialized version of a polyhedron state
    #' @return A new  PolyhedronStateDefined object.
    deserialize = function(serialized.polyhedron) {
      self$state <- PolyhedronStateDeserializer$new(
        serialized.polyhedron
      )
      self$state <- self$state$scrape()
      self$file.id <- serialized.polyhedron$file.id
      # Postprocess in defined state
      self
    },
    #' @description
    #' get Polyhedron name
    #' @return string with polyhedron name
    getName = function() {
      self$state$getName()
    },
    #' @description
    #' Gets polyhedron state
    #' @return A new  PolyhedronState object.
    getState = function() {
      self$state
    },
    #' @description
    #' Gets a solid definition
    #' @return A list of vertex vectors composing polyhedron faces.
    getSolid = function() {
      self$state$getSolid()
    },
    #' @description
    #' checks Edges consistency
    #' @return A boolean value
    isChecked = function() {
      inconsistent.edges <- self$state$checkEdgesConsistency()
      ret <- FALSE
      if (!is.null(inconsistent.edges)) {
        ret <- nrow(inconsistent.edges) == 0
      }
      ret
    },
    #' @description
    #' Return an 'rgl' model with an optional transformation described by transformation.matrix parameter
    #' @param transformation.matrix transformation matrix parameter
    #' @return An tmesh3d object
    getRGLModel = function(transformation.matrix = NULL) {
      logger <- getLogger(self)
      logger$debug("drawing",
        name = self$getName()
      )
      self$state$buildRGL(transformation.matrix = transformation.matrix)
    },
    #' @description
    #' exports an XML definition of current polyhedron
    #' @return A character object with the XML definition
    exportToXML = function() {
      self$state$exportToXML()
    },
    #' @description
    #' returns the errors found when processing current polyhedron
    #' @return a data.frame with polyhedron errors
    getErrors = function() {
      self$state$errors
    },
    #' @description
    #' check properties of current polyhedron
    #' @param expected.vertices expected vertices number
    #' @param expected.faces expected faces number
    #' @return Unmodified polyhedron object
    checkProperties = function(expected.vertices, expected.faces) {
      faces <- self$getSolid()
      testthat::expect_equal(length(faces), expected.faces)
      vertices.solid <- which(row.names(self$state$vertices) %in% unlist(faces))
      testthat::expect_equal(length(vertices.solid), expected.vertices)
      # check Edges consistency
      self$state$checkEdgesConsistency()
      self
    }
  )
)

#' checkVertices()
#'
#' Check rendering vertices properties
#'
#' @param vertices vertices dataframe for checking
#' @param transformed.vertices positioned vertices dataframe for checking
#' @param triangulated.solid triangulated.solid for checking
#' @return checked positioned vertices
#' @importFrom stats runif
#' @noRd
checkVertices <- function(vertices, transformed.vertices, triangulated.solid) {
  triangulated.solid <- sort(unique(unlist(triangulated.solid)))
  set.seed(sum(vertices[, 1:3]))
  transformed.vertices.rows <- intersect(
    triangulated.solid,
    seq_len(nrow(transformed.vertices))
  )
  row <- transformed.vertices.rows[trunc(runif(
    1, 1,
    length(transformed.vertices.rows)
    + 1 - 0.1^9
  ))]
  col <- trunc(runif(1, 1, 4 - 0.1^9))
  transformed.vertices[row, col] <- transformed.vertices[row, col] + 0.1^6
  transformed.vertices
}
