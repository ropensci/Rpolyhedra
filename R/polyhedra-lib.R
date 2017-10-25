#' Gets the list of available polyhedra
#' @import futile.logger
#' @export
getAvailablePolyhedra <- function() {
    ret <- NULL
    if (exists("polyhedra")) {
        ret <- names(polyhedra)
    }
    ret
}

#' Gets a polyhedron from the database.
#' @import futile.logger
#' @export
getPolyhedron <- function(name) {
    ret <- NULL
    if (exists("polyhedra")) {
        ret <- polyhedra[[name]]
    }
    ret
}


#' Get the path of package data
getDataDir <- function(){
  home.dir<-find.package("Rpolyhedra", lib.loc=NULL, quiet = TRUE)
  #TODO find fancy way of accesign data
  data.subdir<-"/extdata/"
  if (length(grep("Resources/library",home.dir))==0)
    data.subdir<-paste("/inst",data.subdir,sep="")
  paste(home.dir, data.subdir, sep = "")
}

#' Get the path of Polyhedra RDS file
getPolyhedraRDSPath <- function(polyhedra.rds.filename="polyhedra.RDS"){
  paste(getDataDir(), polyhedra.rds.filename, sep = "")
}


#' @import stringr rgl
#' @importFrom R6 R6Class
PolyhedronState.class <- R6::R6Class("PolyhedronState", public = list(initialize = function(number, name, symbol, dual,
    vertices, net, hinges, solid, dihedral) {
}, scrape = function(netlib.p3.lines) {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
}, getSolid = function() {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
}, buildRGL = function() {
    stop(gettext("rpoly.abstract_class", domain = "R-Rpolyhedra"))
}))

#' Scrapes each polyhedra from source
#'
#' @import stringr rgl futile.logger
#' @importFrom R6 R6Class
PolyhedronStateScraper.class <- R6::R6Class("PolyhedronStateScraper", inherit = PolyhedronState.class, public = list(netlib.p3.lines = NA,
    labels.rows = NA, labels.map = NA, errors = "", initialize = function(number, netlib.p3.lines) {
        super$initialize()
        self$netlib.p3.lines <- netlib.p3.lines
        self$labels.map <- list()
        self
    }, add.error = function(current.error) {
        self$errors <- paste(self$errors, current.error)
        futile.logger::flog.debug(current.error)
        self$errors
    }, extract_fows_from_label = function(label.number, expected.label) {
        observer.label <- self$netlib.p3.lines[self$labels.rows[label.number]]
        observer.label <- sub("\\:", "", observer.label)
        if (observer.label != expected.label) {
            current.error <- paste(self$errors, "In label#", label.number, "Expected label was", expected.label, " and observed was",
                observer.label)
            self$add.error(current.error)
        }
        first.data.row <- self$labels.rows[label.number]
        last.data.row <- self$labels.rows[label.number + 1]
        ret <- 0
        if (first.data.row > last.data.row) {
            self$add.error(paste("for label", expected.label, "no valid rows: (fr, lr)", first.data.row, last.data.row))
        } else ret <- c((first.data.row + 1):(last.data.row - 1))
        ret
    }, getLabels = function() {
        self$netlib.p3.lines[self$labels.rows]
    }, scrapeNet = function(net.txt, offset = 0) {
        first.line <- strsplit(net.txt[1], split = " ")[[1]]
        faces <- as.numeric(first.line[1])
        max.degree <- as.numeric(first.line[2])
        if (faces != length(net.txt) - 1) self$add.error(paste("declared ", faces, "faces, but having", length(faces) -
            1))
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
        if (vertices.count != length(vertices.txt) - 1) self$add.error(paste("declared ", vertices.count, "vertices.count, but having",
            length(vertices.count) - 1, elements))
        vertices <- data.frame(Pos3D_1 = numeric(), Pos3D_2 = numeric(), Pos3D_3 = numeric(), Pos3D_1_exp = numeric(),
            Pos3D_2_exp = numeric(), Pos3D_3_exp = numeric(), Pos3D_1_exp_text = numeric(), Pos3D_2_exp_text = numeric(),
            Pos3D_3_exp_text = numeric(), stringsAsFactors = FALSE)
        cont <- 1
        n.vertex<-length(vertices.txt)
        for (f in c(2:n.vertex)) {
            cf <- strsplit(vertices.txt[f], " ")[[1]]
            cf.outbrackets <- as.numeric(sapply(cf, FUN = function(x) self$extractCFOutBrackets(x)))
            cf.inbrackets <- stringr::str_extract(cf, "\\[([:graph:]*)\\]")
            cf.inbrackets <- sub("\\[", "", cf.inbrackets)
            cf.inbrackets <- sub("\\]", "", cf.inbrackets)
            futile.logger::flog.debug(paste("parsing vertex ", f,"/",n.vertex,
                                      " ",paste(cf.outbrackets, collapse = ","),
                                      " ",paste(cf.inbrackets, collapse = ","),sep=""))
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
            if (current.label %in% self$labels.map[[current.label]]) stop(paste(gettext("rpoly.row_for_label", domain = "R-Rpolyhedra"),
                current.label, gettext("rpoly.already_defined", domain = "R-Rpolyhedra")))
            self$labels.map[[current.label]] <- r
            futile.logger::flog.debug(paste("Assign order", r, "row", self$labels.rows[r], "to", current.label))
        }
        futile.logger::flog.debug(paste(names(self$labels.map), lapply(self$labels.map, FUN = function(x) self$netlib.p3.lines[self$labels.rows[x]]),
            collapse = "|", sep = "=>"))
        self$labels.map
    }, getDataFromLabel = function(label) {
        r <- self$labels.map[[label]]
        ret <- NULL
        if (!is.null(r)) ret <- self$netlib.p3.lines[self$extract_fows_from_label(r, label)]
        ret
    }, scrape = function() {
        # first check labels
        self$labels.rows <- grep("\\:", self$netlib.p3.lines)
        if (nchar(self$errors) > 0) {
            stop(paste(gettext("rpoly.scraping_issue", domain = "R-Rpolyhedra"), self$errors))
        }
        self$setupLabelsOrder()
        name <- self$getDataFromLabel("name")
        number <- self$getDataFromLabel("number")
        futile.logger::flog.debug(paste("Scraping polyhedron",number,name))
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
            solid <- NA
        }
        # TODO
        hinges <- NULL
        # TODO
        dih <- NULL
        vertices <- self$scrapeVertices(vertices.txt)

        ret <- PolyhedronStateDefined.class$new(number, name, symbol, dual, sfaces, svertices, net, solid, hinges, dih,
            vertices)
        ret
    }, buildRGL = function(size = 1, origin = c(0, 0, 0),normalize.size=TRUE) {
        stop(gettext("rpoly.not_implemented", domain = "R-Rpolyhedra"))
    }))

#' @import stringr rgl
#' @import geometry
#' @importFrom R6 R6Class
PolyhedronStateDefined.class <- R6::R6Class("PolyhedronStateDefined", inherit = PolyhedronState.class, public = list(number = NA,
    name = NA, symbol = NA, dual = NA, sfaces = NA, svertices = NA, net = NA, hinges = NA, solid = NA, dih = NA, vertices = NA,
    vertices.rgl = NULL, solid.triangulated = NULL, mass.center = NA, initialize = function(number, name, symbol, dual, sfaces, svertices, net, solid,
        hinges, dih, vertices) {
        super$initialize()
        self$number <- number
        self$name <- name
        self$symbol <- symbol
        self$dual <- dual
        self$sfaces <- sfaces
        self$svertices <- svertices
        self$net <- net
        self$solid <- solid
        self$hinges <- hinges
        self$dih <- dih
        self$vertices <- vertices
        self
    }, scrape = function(netlib.p3.lines) {
        self
    }, getSolid = function() {
        self$solid
    }, triangulate = function(force=FALSE) {
        if (is.null(self$solid.triangulated)|force){
          net<-self$solid
          self$vertices.rgl <- self$vertices[, 1:3]
          self$vertices.rgl$desc <- "solid"
          faces.size <- unlist(lapply(net, FUN = length))
          max.faces <- max(faces.size)
          # if (max.faces > 3) stop(paste('Not yet implemented for faces with', max.faces))
          f <- 1
          ret <- list()
          for (face in net) {
            current.vertex <- nrow(self$vertices.rgl)
            # the description considers vector referencing starting in 0.
            if (length(face) < 3) {
              stop(paste("Problem with definition. Face defined with", length(face), "vertex"))
            }
            if (length(face) == 3) {
              tmesh <- face
            } else {
              if (length(face) == 4) {
                tmesh <- c(face[1], face[2], face[3], face[3], face[4], face[1])
              }
              if (length(face) >= 5) {
                # debug
                print(paste("big faces", length(face)))
                extra.mid.vertex <- apply(self$vertices[face, 1:3], MARGIN = 2, FUN = mean)
                extra.vertex.id <- current.vertex + 1
                self$vertices.rgl[extra.vertex.id, 1:3] <- extra.mid.vertex
                self$vertices.rgl[extra.vertex.id, 4] <- paste("extra-f", f, sep = "")
                last.v <- length(face)
                tmesh <- NULL
                for (v in 1:length(face)) {
                  tmesh <- c(tmesh, face[last.v], face[v], extra.vertex.id)
                  last.v <- v
                }
              }
            }
            ret[[f]] <- tmesh
            futile.logger::flog.debug(paste("triangulated f", f, length(face), "original", paste(face, collapse = ","),
                                            "triangulated", paste(tmesh, collapse = ",")))
            f <- f + 1
          }
          self$solid.triangulated<-ret
        }
        self$solid.triangulated
    }, getBoundingBox=function(vertex.3d){
      vertex.solid <- self$vertices.rgl[vertex.3d, c(1:3)]
      vertex.solid.min<-apply(vertex.solid,MARGIN = 2,FUN=min)
      vertex.solid.max<-apply(vertex.solid,MARGIN = 2,FUN=max)
      rbind(vertex.solid.min,vertex.solid.max)
    }, calculateMassCenter = function(size = 1, vertex.3d) {
        resized.vertex <- self$vertices.rgl[vertex.3d, c(1:3)] * size
        # delaunayn(resized.vertex)
        self$mass.center <- apply(resized.vertex, MARGIN = 2, FUN = mean)
        self$mass.center
    }, buildRGL = function(size = 1, origin = c(0, 0, 0),normalize.size=TRUE, ...) {
        ret <- NULL
        if (length(self$solid) > 1) {
            net <- self$triangulate()
            if (normalize.size){
              bounding.box<-self$getBoundingBox(vertex.3d=sort(unique(unlist(net))))
              volume<-prod(apply(bounding.box,MARGIN=2,FUN=function(x){x[2]-x[1]}))
              #0.7501087 is tetrahedron bounding box
              size<-size*(0.7501087/volume)^(1/3)
            }
            self$calculateMassCenter(size, vertex.3d = sort(unique(unlist(net))))
            positioned.vertex <- self$vertices.rgl[, c(1:3)] * size
            for (c in 1:3) {
                positioned.vertex[, c] <- positioned.vertex[, c] + origin[c] - self$mass.center[c]
            }
            vertices <- as.matrix(cbind(positioned.vertex, 1))
            ret <- rgl::tmesh3d(c(t(vertices)), unlist(net), ...)
        } else {
            futile.logger::flog.info(paste("For", self$name, " solid definition not found"))
        }
        ret
    }))

#' Processes the polyhedron in order to produce a graphical representation.
#'
#' @import stringr rgl futile.logger
#' @importFrom R6 R6Class
Polyhedron.class <- R6::R6Class("Polyhedron", public = list(number = NA, state = NA, initialize = function(number, state = NULL) {
    self$number <- number
    if (!is.null(state)) {
        self$state <- state
    }
}, scrape = function(netlib.p3.lines) {
    self$state <- PolyhedronStateScraper.class$new(self$number, netlib.p3.lines)
    self$state <- self$state$scrape()
    self
}, getName = function() {
    self$state$name
}, getSolid = function() {
    self$state$getSolid()
}, getRGLModel = function(size = 1, origin = c(0, 0, 0)) {
    futile.logger::flog.debug(paste("drawing", self$getName()))
    self$state$buildRGL(size = size, origin = origin)
}))


scrapePolyhedra <- function(max.quant = 0, home.dir.data = getDataDir(),
                            test = TRUE, save.rds.force = FALSE) {
    futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "data.io")
    futile.logger::flog.threshold(futile.logger::DEBUG)

    # wget -r -np -k http://www.netlib.org/polyhedra/ data/www.netlib.org/
    polyhedra.dir <- paste(home.dir.data, "www.netlib.org/polyhedra/", sep = "")
    # debug
    polyhedra.rds.file<-getPolyhedraRDSPath()
    futile.logger::flog.debug(paste("opening", polyhedra.rds.file))
    polyhedra.files <- dir(polyhedra.dir)
    polyhedra.files <- polyhedra.files[grep("[0-9]+", polyhedra.files)]
    polyhedra.files <- polyhedra.files[order(as.numeric(polyhedra.files))]
    polyhedra <- list()
    save.rds <- !test | save.rds.force
    i <- 1
    if (file.exists(polyhedra.rds.file) == TRUE) {
        polyhedra.saved <- readRDS(polyhedra.rds.file)
        if (!save.rds.force) {
            save.rds <- FALSE
        }
    }
    if (max.quant > 0) {
        polyhedra.files <- polyhedra.files[1:max.quant]
    }
    if (save.rds | test) {
        # Only run scraping if has to save.rds or test
        for (polyhedra.file in polyhedra.files) {
            current.file <- paste(polyhedra.dir, polyhedra.file, sep = "")
            polyedra.netlib.lines <- readLines(current.file)
            current.polyhedron <- Polyhedron.class$new(polyhedra.file)
            current.polyhedron$scrape(netlib.p3.lines = polyedra.netlib.lines)
            futile.logger::flog.debug(paste("parsed", polyhedra.file, "with name", current.polyhedron$state$name))
            current.polyhedron$getRGLModel(1, c(0, 0, 0))
            futile.logger::flog.debug(paste("generated RGLModel"))
            polyhedra[[current.polyhedron$getName()]] <- current.polyhedron
            if (exists("polyhedra.saved") & test) {
                # TODO add equals in polyedraR6 object
                polyhedron.name <- current.polyhedron$getName()
                expect_true(polyhedron.name %in% names(polyhedra.saved))
                expect_equal(polyhedra.saved[[polyhedron.name]], current.polyhedron)
            }
            i <- i + 1
        }
    }
    if (save.rds == TRUE) {
        # only save if iterates all polyhedra
        saveRDS(polyhedra, polyhedra.rds.file)
    }
    polyhedra
}
