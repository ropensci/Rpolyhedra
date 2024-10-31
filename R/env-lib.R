#' Get variable from package environment
#'
#' Obtains a variable from package environment
#' @param variable.name  name of variable to be retrieved
#' @noRd
getPackageEnvir <- function(variable.name) {
  get(variable.name, envir = asNamespace("Rpolyhedra"))
}

#' Set package environment variable
#'
#' Set a variable from package environment
#'
#' @noRd
#' @param variable.name  name of variable to be set
#' @param value          variable value
setPackageEnvir <- function(variable.name, value) {
  assign(x = variable.name, value = value, envir = asNamespace("Rpolyhedra"))
}


#' Get an user environment variable
#'
#' Gets a writable environment for the package
#'
#' @param variable.name  name of variable to be retrieved
#' @noRd
getUserEnvir <- function(variable.name) {
  get(x = variable.name, envir = getPackageEnvir("RpolyhedraEnv"))
}

#' Set an user environment variable
#'
#' Sets variable in the writable environment for the package
#'
#' @param variable.name  name of variable to be set
#' @param value          variable value
#' @noRd
setUserEnvir <- function(variable.name, value) {
  assign(
    x = variable.name, value = value, envir =
      getPackageEnvir("RpolyhedraEnv")
  )
}


#' Get data environment
#'
#' Gets the current .data.env value
#'
#' @return .data.env
#' @noRd
getDataEnv <- function() {
  getUserEnvir(".data.env")
}


#' Get user space
#'
#' This function is used internally for accessing the local database path
#' @return path of user space
#' @noRd
getUserSpace <- function() {
  file.path(path.expand("~"), ".R", "Rpolyhedra")
}

#' Initialize data directory environment
#'
#' initialize data environment
#'
#' @return the data dir environment
#' @noRd
initDataDirEnvironment <- function() {
  environment.filepath <- getEnvironmentFilepath()
  if (!file.exists(environment.filepath)) {
    .data.env <- "PACKAGE"
  } else {
    .data.env <- readLines(environment.filepath)[1]
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}

#' Get data directory
#'
#' Gets the path of Rpolyhedra data dir.
#'
#' This function is used internally to determine whether the package
#' is compiled in source or package directory.
#' @param data.env environment where data directory must be returned
#' @return dir where the package access polyhedra database
#' @noRd
getDataDir <- function(data.env = getDataEnv()) {
  data.dir <- ""
  if (data.env == "HOME") {
    data.dir <- getUserSpace()
  } else {
    data.dir <- getPackageDir()
  }
  data.dir
}

#' Get environment file path
#'
#' Gets the filename where package data environment is persisted
#' @return The environment filepath
#' @noRd
getEnvironmentFilepath <- function() {
  file.path(getDataDir("HOME"), "Rpolyhedra.env")
}

#' Set data directory environment
#'
#' Sets the path of Rpolyhedra data dir.
#'
#' This function is used to set the data directories either to the package or the user home directory.
#'
#' @param env The type of environment to work with. Values are "PACKAGE" or "HOME" and it defaults to package
#' @return the current .data.env
#' @noRd
setDataDirEnvironment <- function(env = "PACKAGE") {
  if (env %in% c("PACKAGE", "HOME")) {
    .data.env <- env
  } else {
    stop("Possible values are PACKAGE and HOME")
  }
  if (file.exists(getUserSpace())) {
    write(.data.env, getEnvironmentFilepath())
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}

#' Get package directory
#'
#' Gets the path of package data.
#' @noRd
getPackageDir <- function() {
  home.dir <- find.package("Rpolyhedra", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata")
  if (!dir.exists(file.path(home.dir, data.subdir))) {
    data.subdir <- "extdata"
  }
  file.path(home.dir, data.subdir)
}

#' Get polyhedra RDS file path
#'
#' Gets the path of Polyhedra RDS database file
#'
#' @param polyhedra.rds.filename filename of polyhedra database
#' @return the path to the Polyhedra database file
#' @noRd
getPolyhedraRDSPath <- function(polyhedra.rds.filename = "polyhedra.RDS") {
  file.path(getDataDir(), polyhedra.rds.filename)
}

#' Select data environment
#'
#' Asks the user where to set the system variable .data.env
#'
#' @param env The environment to run on, can be PACKAGE, HOME or NULL. If null, it asks the user for a an Environment.
#' @param prompt.value If specified, no prompt is shown
#' @param downloadDatabase If specified, when changing to HOME env, downloads the database from fulldb repo.
#' @usage
#'     selectDataEnv(env=NA, prompt.value = NULL)
#' @return .data.env
#' @import lgr
#' @noRd
selectDataEnv <- function(env = NA, downloadDatabase = TRUE,
                          prompt.value = NULL,
                          logger = lgr) {
  retVal <- "SUCCESS"
  if (is.na(env)) {
    if (!is.na(Sys.getenv(x = "ON_TRAVIS", unset = NA))) {
      return(TRUE)
    }
    if (is.null(prompt.value)) {
      prompt.value <- readline(
        prompt = paste(
          "Full Database needs to download data to home folder. ",
          "Agree [y/n]?:"
        )
      )
    }

    retry <- TRUE
    while (retry) {
      answer <- tolower(prompt.value[1])
      if (answer == "n") {
        logger$info(paste(
          "Working on demo DB. You can call",
          "selectDataEnv to use the full database."
        ))
        setDataDirEnvironment("PACKAGE")
        retry <- FALSE
      } else if (answer == "y") {
        setDataDirEnvironment("HOME")
        retry <- FALSE
      } else {
        retry <- TRUE
      }
      if (retry) {
        prompt.value <- readline(prompt = "Unknown option. Agree [y/n]?:")
      }
    }
  } else {
    setDataDirEnvironment(env)
  }
  .data.env <- getDataEnv()
  # loads the database
  if (.data.env == "HOME") {
    # create dir
    data.dir <- getUserSpace()
    if (!dir.exists(data.dir)) {
      dir.create(data.dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (downloadDatabase) {
      retVal <- downloadRPolyhedraSupportingFiles()
    }
  }

  if (retVal == "SUCCESS") {
    updatePolyhedraDatabase()
  } else {
    setDataDirEnvironment("PACKAGE")
  }
  getDataEnv()
}

#' Get git commit
#'
#' get the last git commit sha
#' @param long.version determines if the complete version of the sha will
#'         be returned.
#' @return String with git commit sha
#' @noRd
getGitCommit <- function(long.version = FALSE) {
  if (file.exists(".git")) {
    git.sha <- system("git log --pretty=format:'%h' -n 1", intern = TRUE)[1]
  } else {
    git.sha <- NA
  }
  if (long.version == FALSE) {
    git.sha <- substr(git.sha, 1, 7)
  }
  git.sha
}

#' genLogger
#' @description
#' Returns a configured logger with threshold according r6 object.
#' This function is usually called in class constructors
#' @param r6.object an r6.object
#'
#' @author ken4rab
#' @export
genLogger <- function(r6.object) {
  lgr::get_logger(class(r6.object)[[1]])
}

#' getLogger
#' @description
#' Returns the configured lgr of an r6 object.
#' If the object don't have a lgr or is not initialized returns an error
#' @param r6.object an r6.object
#'
#' @author ken4rab
#' @export
getLogger <- function(r6.object) {
  ret <- r6.object$logger
  if (is.null(ret)) {
    class <- class(r6.object)[[1]]
    stop(paste("Class", class, "don't seems to have a configured logger"))
  } else {
    ret.class <- class(ret)[[1]]
    if (ret.class == "logical") {
      stop(paste("Class", ret.class, "needs to initialize logger: self$logger <- genLogger(self)"))
    }
  }
  ret
}


#' loggerSetupFile
#' @param log.file log path for logging file
#' @param default.threshold threshold for setting root. Default = "info"
#' @param append if set to FALSE, cleanup all previous logs
#' @import lgr
#' @author kenarab
#' @export
loggerSetupFile <- function(log.file, default.threshold = "info", append = TRUE) {
  if (!append){
    unlink(unique(unlist(lapply(lgr$appenders, FUN = function(x){x$file}))))  # cleanup
  }
  lgr::basic_config()
  lgr::get_logger("root")$add_appender(AppenderFile$new(log.file,
                                                        layout = LayoutFormat$new(
                                                          fmt = "%L [%t] %m %j",
                                                          timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
                                                          colors = NULL,
                                                          pad_levels = "right"
                                                        )
  ))
  lgr::threshold(default.threshold, lgr::get_logger("root"))
  lgr
}

#' mutate_cond
#' @param .data data frame to apply the mutate
#' @param condition condition to conditionally apply mutate
#' @param ... mutation function
#' @param envir environment to apply condition
#' @import dplyr
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


