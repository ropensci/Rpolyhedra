#' getPackageEnvir
#'
#' @param variable.name  name of variable to be retrieved

#' Obtains a variable from package environment
getPackageEnvir <- function(variable.name){
  get(variable.name, envir = asNamespace("Rpolyhedra"))
}

#' setPackageEnvir
#'
#' Set a variable from package environment
#' @param variable.name  name of variable to be set
#' @param value          variable value

setPackageEnvir <- function(variable.name, value){
  assign(x = variable.name, value = value, envir = asNamespace("Rpolyhedra"))
}


#' getUserEnvir
#'
#' Gets a writable environment for the package
#'
#' @param variable.name  name of variable to be retrieved
getUserEnvir <- function(variable.name) {
  get(x = variable.name, envir = getPackageEnvir("RpolyhedraEnv"))
}

#' setUserEnvir
#'
#' Sets variable in the writable environment for the package
#'
#' @param variable.name  name of variable to be set
#' @param value          variable value

setUserEnvir <- function(variable.name, value) {
  assign(x = variable.name, value = value, envir =
           getPackageEnvir("RpolyhedraEnv"))
}


#' getDataEnv
#'
#' Gets the current .data.env value
#'
#' @return .data.env
getDataEnv <- function() {
  getUserEnvir(".data.env")
}


#' getUserSpace
#'
#' This function is used internally for accesing the local database path
#' @return path of user space
getUserSpace <- function(){
  file.path(path.expand("~"), ".R", "Rpolyhedra")
}

#' initDataDirEnvironment
#'
#' initialize data enviornment
#'
#' @return the data dir environment
initDataDirEnvironment <- function() {
  environment.filepath <- getEnvironmentFilepath()
  if (!file.exists(environment.filepath)){
    .data.env <- "PACKAGE"
  } else {
    .data.env <- readLines(environment.filepath)[1]
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}




#' getDataDir
#'
#' Gets the path of Rpolyhedra data dir.
#'
#' This function is used internally to determine whether the package
#' is compiled in source or package directory.
#' @param data.env enviroment where data directory must be returned
#' @return dir where the package access polyhedra database

getDataDir <- function(data.env = getDataEnv()) {
  data.dir <- ""
  if (data.env == "HOME") {
    data.dir <- getUserSpace()
  } else {
    data.dir <- getPackageDir()
  }
  data.dir
}

#' getEnvironmentFilepath
#'
#' Gets the filename where package data environment is persisted
#' @return The environment filepath

getEnvironmentFilepath <- function(){
  file.path(getDataDir("HOME"), "Rpolyhedra.env")
}

#' setDataDirEnvironment
#'
#' Sets the path of Rpolyhedra data dir.
#'
#' This function is used to set the data directories either to the package or the user home directory.
#'
#' @param env The type of environment to work with. Values are "PACKAGE" or "HOME" and it defaults to package
#' @return the curruent .data.env
setDataDirEnvironment <- function(env = "PACKAGE") {
  if (env %in% c("PACKAGE", "HOME")) {
    .data.env <- env
  }
  else {
    stop("Possible values are PACKAGE and HOME")
  }
  if (file.exists(getUserSpace())) {
    write(.data.env, getEnvironmentFilepath())
  }

  setUserEnvir(".data.env", value = .data.env)
  .data.env
}




#' getPackageDir
#'
#' Gets the path of package data.
getPackageDir <- function(){
  home.dir <- find.package("Rpolyhedra", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata/")
  if (!dir.exists(file.path(home.dir, data.subdir)))
    data.subdir <- "extdata/"
  file.path(home.dir, data.subdir)
}

#' getPolyhedraRDSPath
#'
#' Gets the path of Polyhedra RDS database file
#'
#' @param polyhedra_rds_filename filename of polyhedra database
#' @return the path to the Polyhedra database file
getPolyhedraRDSPath <- function(polyhedra_rds_filename = "polyhedra.RDS") {
  file.path(getDataDir(), polyhedra_rds_filename)
}

#' selectDataEnv
#'
#' Asks the user where to set the system variable .data.env
#'
#' @param env The environment to run on, can be PACKAGE, HOME or NULL. If null, it asks the user for a an Environment.
#' @usage
#'     selectDataEnv(env=NA)
#' @return .data.env
#' @importFrom futile.logger flog.info
selectDataEnv <- function(env=NA) {
  retVal <- "SUCCESS"
  if (is.na(env)) {
    if (!is.na(Sys.getenv(x = "ON_TRAVIS", unset = NA))) {
      return(TRUE)
    }
    accept.option <- readline(
      prompt = paste("Full Database needs to download data to home folder. ",
                     "Agree [y/n]?:"))
    retry <- TRUE
    while (retry) {
      answer <- tolower(accept.option[1])
      if (answer == "n") {
        futile.logger::flog.info(paste("Working on demo DB. You can call",
                  "selectDataEnv to use the full database."))
        setDataDirEnvironment("PACKAGE")
        retry <- FALSE
      } else if (answer == "y") {
        setDataDirEnvironment("HOME")
        retry <- FALSE
      }
      else {
        retry <- TRUE
      }
      if (retry) {
        accept.option <- readline(prompt = "Unknown option. Agree [y/n]?:")
      }
    }
  } else {
    setDataDirEnvironment(env)
  }
  .data.env <- getDataEnv()
  #loads the database
  if (.data.env == "HOME"){
    #create dir
    data.dir <- getUserSpace()
    if (!dir.exists(data.dir)) {
      dir.create(data.dir, recursive = TRUE, showWarnings = FALSE)
    }
    retVal <- downloadRPolyhedraSupportingFiles()
  }

  if (retVal == "SUCCESS") {
    updatePolyhedraDatabase()
  } else {
    setDataDirEnvironment("PACKAGE")
  }


  retVal
}


#' getGitCommit
#' get the last git commit sha
#' @param long.version determines if the complete version of the sha will
#'         be returned.
#' @importFrom git2r commits
#' @return String with git commit sha
#'
getGitCommit <- function(long.version = FALSE){
  #TODO: replace with git2r when issue #2 is resolved.
  #git2r::commits()[[1]]@sha
  if (file.exists(".git")){
    git.sha <- system("git log --pretty=format:'%h' -n 1", intern = TRUE)[1]
  }
  else{
    git.sha <- NA
  }
  if (long.version == FALSE) {
    git.sha <- substr(git.sha, 1, 7)
  }
  git.sha
}
