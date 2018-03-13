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
#'   \item{\code{updateStatus(source, filename, status, status.field = 'status', scraped.name = NA, obs ='')}}{Updates status of source and filenames parameters in Ledger }
#'   \item{\code{savePreloadedComplexities()}}{Internal method which saves a file with an estimation of time required time to scrape each filename}
#'   \item{\code{loadPreloadedComplexities()}}{Load a file with an estimation of time required time to scrape each filename}
#'   \item{\code{getSizeToTimeScrape(sources, time2scrape = 60)}}{Estimates how much filenames could be scraped in a time frame, considering data retrieved with loadPreloadedComplexities}
#'   \item{\code{resetStatesMetrics()}}{Reset metrics of application of different status values}
#'   \item{\code{countStatusUse(status.field,status)}}{Add an use to the metrics of status.field and status parameters}
#'   \item{\code{getFilenamesStatusMode(mode,sources = sort(unique(self$df$source)),max.quant = 0,order.by.time2scrape = FALSE)}}{Get a list of the filenames in the ledger with a defined mode (status agrupation)}
#'   \item{\code{getFilenamesStatus(status,sources = sort(unique(self$df$source)),max.quant = 0,order.by.time2scrape = FALSE)}}{Get a list of the filenames in the ledger with specified status}
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
   preloaded.complexities.filename = NA,
   preloaded.complexities = NA,
   initialize = function() {
     self$df <- data.frame(id           = character(),
                           source       = character(),
                           number       = numeric(),
                           filename     = character(),
                           start.scrape = as.POSIXct(character()),
                           end.scrape   = as.POSIXct(character()),
                           status       = character(),
                           scraped.name = character(),
                           status.test  = character(),
                           obs          = character(),
                           git.commit   = character(),
                           time.scraped = numeric(),
                           preloaded.time2scrape = numeric(),
                           stringsAsFactors = FALSE)
     self$resetStatesMetrics()
     self$loadPreloadedComplexities()
     self
   },
   addFilename = function(source, filename){
     r <- NULL
     default.status <- "queued"
     if (is.null(self$getIdFilename(source, filename))){
       r <- nrow(self$df)+1
       status.field <- "status"
       self$countStatusUse(status.field = status.field, status = default.status)
       states.row <- which(self$states$status.field %in% status.field &
                             self$states$status %in% default.status)
       number <- self$states[states.row,"count"]

       #obtain preloaded.time2scrape
       row.preloaded.t2s <- which(self$preloaded.complexities$source == source &
                                    self$preloaded.complexities$filename==filename)
       if (length(row.preloaded.t2s)==1){
         preloaded.time2scrape <- as.numeric(self$preloaded.complexities[row.preloaded.t2s,"time2scrape"])
       }
       else{
         preloaded.time2scrape <- NA
       }
       self$df[r,c("id","source","filename","status")]<- c(r,source,filename,default.status)
       self$df[r,c("number","preloaded.time2scrape")]<- c(number,preloaded.time2scrape)
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
                           scraped.name = NA, obs =""){
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
         fields.update <- c("end.scrape", "scraped.name", "git.commit")
         start.scrape  <- self$df[retrieved.id,"start.scrape"]
         time.scraped  <- round(as.numeric(end.scrape - start.scrape),3)
         values.update <- c(as.character(end.scrape), scraped.name, getGitCommit())
         #in a different commands for not converting it to character
         self$df[retrieved.id,"time.scraped"] <- time.scraped
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
     ret
   },
   savePreloadedComplexities = function(){
     preloaded.complexities <- self$df[!is.na(self$df$time.scraped),c("source","filename","time.scraped")]
     preloaded.complexities <- preloaded.complexities[order(preloaded.complexities$time.scraped,
                                                            preloaded.complexities$source,
                                                            preloaded.complexities$filename),]
     names(preloaded.complexities)[3]<-"time2scrape"
     write.csv(preloaded.complexities, self$preloaded.complexities.filename,
               row.names = FALSE)
     preloaded.complexities
   },
   loadPreloadedComplexities = function(){
     self$preloaded.complexities.filename <- paste(getDataDir(),"polyhedra.complexity.csv",sep="")
     self$preloaded.complexities <- read.csv(paste(getDataDir(),"polyhedra.complexity.csv",sep=""))
     self$preloaded.complexities
   },
   getSizeToTimeScrape = function(sources, time2scrape = 60){
     pre.comp.source <- self$preloaded.complexities[self$preloaded.complexities$source %in% sources,]
     pre.comp.source <- pre.comp.source[order(pre.comp.source$time2scrape,
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
                                     order.by.time2scrape = FALSE){
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
                             order.by.time2scrape = order.by.time2scrape)
   },
   getFilenamesStatus = function(status,
                                 sources = sort(unique(self$df$source)),
                                 max.quant = 0,
                                 order.by.time2scrape = FALSE){
     filtred.rows <- which(self$df$source %in% sources &
                             self$df$status %in% status)
     ret <- NULL
     if (length(filtred.rows)>0){
       if (max.quant >0){
         filtred.rows <- filtred.rows[1:min(max.quant,length(filtred.rows))]
       }
       ret <- self$df[filtred.rows,]
       if (order.by.time2scrape){
         ret <- ret[order(ret$preloaded.time2scrape,ret$source,ret$number),]
       }
     }
     ret
   }
 ))
