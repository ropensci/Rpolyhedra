
#' maxWithoutNA
#' Function that returns NA if all elements are NA, and the max value not NA, if not.
#' @param x vector parameter

maxWithoutNA <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=TRUE), NA)


#' ScraperLedger
#'
#' Ledger of scraping status of each objects. Allows different type of states:
#' queued, scraping, scraped, failed, exception, skipped
#'
#' For flexible and reproducible configuration for package development
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{initializes the object}
#'   \item{\code{addFilename(source, filename)}}{add filename to the ledger}
#'   \item{\code{getIdFilename(source, filename)}}{Returns id/row of source and filenames parameters in the ledger}
#'   \item{\code{updateStatus(source, filename, status, status.field = 'status', scraped.polyhedron = NA, obs ='')}}{Updates status of source and filenames parameters in Ledger }
#'   \item{\code{savePreloadedData()}}{Internal method which saves a file with an estimation of time required time to scrape each filename}
#'   \item{\code{loadPreloadedData()}}{Load a file with an estimation of time required time to scrape each filename}
#'   \item{\code{getSizeToTimeScrape(sources, time2scrape = 60)}}{Estimates how much filenames could be scraped in a time frame, considering data retrieved with loadPreloadedData}
#'   \item{\code{resetStatesMetrics()}}{Reset metrics of application of different status values}
#'   \item{\code{countStatusUse(status.field,status)}}{Add an use to the metrics of status.field and status parameters}
#'   \item{\code{getFilenamesStatusMode(mode,sources = sort(unique(self$df$source)),max.quant = 0,order.by.vertices.faces = FALSE)}}{Get a list of the filenames in the ledger with a defined mode (status agrupation)}
#'   \item{\code{getFilenamesStatus(status,sources = sort(unique(self$df$source)),max.quant = 0,order.by.vertices.faces = FALSE)}}{Get a list of the filenames in the ledger with specified status}
#' }
#'
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @import futile.logger
#' @importFrom R6 R6Class
#'

ScraperLedger.class <- R6::R6Class("ScraperLedger",
 public = list(
   states = NULL,
   df = NA,
   dirty = FALSE,
   preloaded.data.filename = NA,
   preloaded.data = NA,
   initialize = function() {
     self$df <- data.frame(id           = character(),
                           source       = character(),
                           number       = numeric(),
                           filename     = character(),
                           start.scrape = as.POSIXct(character()),
                           end.scrape   = as.POSIXct(character()),
                           status       = character(),
                           scraped.name = character(),
                           scraped.vertices   = numeric(),
                           scraped.faces      = numeric(),
                           status.test        = character(),
                           obs                = character(),
                           git.commit         = character(),
                           time.scraped       = numeric(),
                           preloaded.name        = character(),
                           preloaded.vertices    = numeric(),
                           preloaded.faces       = numeric(),
                           preloaded.time2scrape = numeric(),
                           stringsAsFactors = FALSE)
     self$resetStatesMetrics()
     self$loadPreloadedData()
     self
   },
   addFilename = function(source, filename){
     r <- NULL
     default.status <- "queued"
     if (is.null(self$getIdFilename(source, filename))){
       futile.logger::flog.info(paste("Adding Filename to ledger ",source, filename))
       r <- nrow(self$df)+1
       status.field <- "status"
       self$countStatusUse(status.field = status.field, status = default.status)
       states.row <- which(self$states$status.field %in% status.field &
                             self$states$status %in% default.status)
       number <- self$states[states.row,"count"]

       #obtain preloaded.time2scrape
       row.preloaded.t2s <- which(self$preloaded.data$source == source &
                                    self$preloaded.data$filename==filename)
       if (length(row.preloaded.t2s)==1){
         preloaded.data <- self$preloaded.data[row.preloaded.t2s,]
         self$df[r,c("preloaded.name")]<-
           as.character(preloaded.data[,3])
         self$df[r,c("preloaded.vertices","preloaded.faces","preloaded.time2scrape")]<-
           c(preloaded.data[,4:6])
       }
       else{
         preloaded.time2scrape <- NA
       }
       self$df[r,c("id","source","filename","status")]<- c(r,source,filename,default.status)
       self$df[r,c("number")]<- c(number)
       self$dirty <- TRUE
     }
     r
   },
   getIdFilename = function(source, filename){
     r <- which(self$df$source == source & self$df$filename == filename)
     if (length(r)>0){
       self$df[r,"id"]
     }
     else{
       r <- NULL
     }
     r
   },
   updateStatus = function(source, filename, status, status.field = "status",
                           scraped.polyhedron = NA, obs =""){
     if (is.null(obs)){
       obs <- ""
     }
     if (!status.field %in% c("status","status.test"))
       stop(paste("Cannot update invalid status field",status.field))
     ret <- NULL
     retrieved.id <- self$getIdFilename(source = source, filename = filename)
     if (length(retrieved.id)!=1){
       stop(paste("There must be a unique row for",source,filename,"and have",length(retrieved.id)))
     }
     if (status.field == "status"){
       end.scrape <- Sys.time()
       if (status == "scraping"){
         fields.update <- c("start.scrape")
         values.update <- as.character(end.scrape)
       }
       if (status %in% c("scraped","failed","skipped","exception")){
         start.scrape  <- self$df[retrieved.id,"start.scrape"]
         time.scraped  <- round(as.numeric(end.scrape - start.scrape),3)

         fields.update <- c("end.scrape",  "git.commit")
         values.update <- c(as.character(end.scrape), getGitCommit())
         fields.numeric.update <-c("time.scraped")
         values.numeric.update <- c(time.scraped)
         #in a different commands for not converting it to character
         if (status %in% "scraped"){
           #TODO Check scraped.name = preloaded name
           scraped.name <- scraped.polyhedron$getName()
           scraped.vertices <- nrow(scraped.polyhedron$getState()$getVertices(solid = TRUE))
           scraped.faces    <- length(scraped.polyhedron$getState()$getSolid())

           fields.update <- c(fields.update, "scraped.name")
           values.update <- c(values.update, scraped.name)
           fields.numeric.update <-c(fields.numeric.update, "scraped.vertices", "scraped.faces")
           values.numeric.update <- c(values.numeric.update,scraped.vertices,scraped.faces)
         }
         self$df[retrieved.id, fields.numeric.update] <-values.numeric.update
       }
     }
     if (status.field == "status.test"){
       #status.test only possible value
       if (status %in% c("tested","testing","failed")){
         fields.update <- NULL
         values.update <- NULL
       }
     }

     fields.update  <- c(fields.update,status.field,"obs")
     values.update  <- c(values.update,status,obs)

     self$df[retrieved.id,fields.update] <- values.update
     ret <- self$df[retrieved.id,]
     #count status uses
     self$countStatusUse(status.field,status)
     self$dirty <- TRUE
     ret
   },
   updateCalculatedFields = function(){
     resolveScrapedPreloaded <- function(x, field){maxWithoutNA(c(x[paste("scraped",field,sep=".")],
                                                                  x[paste("preloaded",field,sep=".")]))}
     if (self$dirty){
       self$df$name     <-  apply(self$df,MARGIN = 1, FUN=function(x){resolveScrapedPreloaded(x=x, field="name")} )
       self$df$vertices <-  as.numeric(apply(self$df,MARGIN = 1, FUN=function(x){resolveScrapedPreloaded(x=x, field="vertices")}))
       self$df$faces    <-  as.numeric(apply(self$df,MARGIN = 1, FUN=function(x){resolveScrapedPreloaded(x=x, field="faces")}))
       self$dirty <- FALSE
     }
   },
   getAvailablePolyhedra = function(sources = sources,
                                    search.string = search.string,
                                    ignore.case = ignore.case){
     self$updateCalculatedFields()
     ret <- self$df[!is.na(self$df$name) & self$df$source %in% sources ,
                    c("source","name","vertices","faces","status")]

     if (!is.null(search.string)) {
       ret <- ret[grepl(search.string, ret$name,ignore.case = ignore.case),]
     }
     ret <- ret[order(ret$vertices,ret$faces,ret$source),]
     ret
   },
   savePreloadedData = function(){
     preloaded.data <- self$df[!is.na(self$df$time.scraped),c("source","filename","scraped.name","scraped.vertices","scraped.faces","time.scraped")]
     preloaded.data <- preloaded.data[order(preloaded.data$time.scraped,
                                                            preloaded.data$source,
                                                            preloaded.data$filename),]
     names(preloaded.data)[3:6]<-c("name","vertices","faces","time2scrape")
     write.csv(preloaded.data, self$preloaded.data.filename,
               row.names = FALSE)
     preloaded.data
   },
   loadPreloadedData = function(){
     self$preloaded.data.filename <- paste(getDataDir(),"polyhedra.preloaded.data.csv",sep="")
     self$preloaded.data <- read.csv(paste(getDataDir(),"polyhedra.preloaded.data.csv",sep=""))
     self$preloaded.data
   },
   getSizeToTimeScrape = function(sources, time2scrape = 60){
     pre.comp.source <- self$preloaded.data[self$preloaded.data$source %in% sources,]
     pre.comp.source <- pre.comp.source[order(pre.comp.source$vertices,
                                              pre.comp.source$faces,
                                              pre.comp.source$source,
                                              pre.comp.source$filename),]
     pre.comp.source$cummsum <- cumsum(pre.comp.source$time2scrape)
     length(which(pre.comp.source$cummsum<time2scrape))
   },
   resetStatesMetrics = function(){
     self$states <- data.frame(status.field = character(),
                               status       = character(),
                               count        = numeric(),
                               stringsAsFactors = FALSE)
     self$states
   },
   countStatusUse = function(status.field,status){
     status.row <- which(self$states$status.field %in% status.field &
                           self$states$status %in% status)
     if (length(status.row)==0){
       status.row <- nrow(self$states) + 1
       count <- 1
     }
     else {
       count <- self$states[status.row,"count"]+1
     }
     self$states[status.row, c("status.field","status")] <- c(status.field,status)
     self$states[status.row, "count"] <- count
     self$states
   },
   getFilenamesStatusMode = function(mode,
                                     sources = sort(unique(self$df$source)),
                                     max.quant = 0,
                                     order.by.vertices.faces = FALSE){
     #status in queued, scraped, exception, retry, skipped
     allowed.status<- NULL
     if (mode == "scrape.retry"){
       allowed.status <- c("queued","exception","failed")
     }
     if (mode == "scrape.queued"){
       allowed.status <- c("queued","scraping")
     }
     if (mode == "test"){
       allowed.status <- "scraped"
     }
     if (mode == "skipped"){
       #special mode skipped for intentionally avoid scraping
       allowed.status <- c("skipped","scraping")
     }
     self$getFilenamesStatus(status = allowed.status,
                             sources = sources,
                             max.quant = max.quant,
                             order.by.vertices.faces = order.by.vertices.faces)
   },
   getFilenamesStatus = function(status,
                                 sources = sort(unique(self$df$source)),
                                 max.quant = 0,
                                 order.by.vertices.faces = FALSE){
     self$updateCalculatedFields()
     filtred.rows <- which(self$df$source %in% sources &
                             self$df$status %in% status)
     ret <- NULL
     if (length(filtred.rows)>0){
       if (max.quant >0){
         filtred.rows <- filtred.rows[1:min(max.quant,length(filtred.rows))]
       }
       ret <- self$df[filtred.rows,]
       if (order.by.vertices.faces){
         ret <- ret[order(ret$vertices,ret$faces,ret$source,ret$name),]
       }
     }
     ret
   }
 ))

#' checkVertices
#'
#' Check rendering vertices properties
#'
checkVertices <- function(vertices, positioned.vertices, triangulated.solid){
  triangulated.solid <- sort(unique(unlist(triangulated.solid)))
  set.seed(sum(vertices[,1:3]))
  positioned.vertices.rows <- intersect(triangulated.solid,1:nrow(positioned.vertices))
  row <- positioned.vertices.rows[trunc(runif(1,1,length(positioned.vertices.rows)+1-0.1^9))]
  col <- trunc(runif(1,1,4-0.1^9))
  positioned.vertices[row,col] <-positioned.vertices[row,col]+0.1^6
  positioned.vertices
}
