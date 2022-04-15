library(Rpolyhedra)
library(stringr)
library(lgr)
library(rgl)
library(geometry)
library(testthat)

# Change threshold to ERROR. Comment out/change if verbosity required for development
lgr::basic_config(threshold = "ERROR")
#' getDataDirMockedTest mocked function for a temp dest folder for testing proposes



testthat::test_check("Rpolyhedra")
