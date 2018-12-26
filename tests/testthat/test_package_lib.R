context("package-lib")
test_that("test on package lib functions", {
  testthat::expect(!is.null(getPreloadedDataFilename()),
                   failure_message = "getPreloadedDataFilename cannot be null")
  testthat::expect(!is.null(updatePolyhedraDatabase()),
                   failure_message = "updatePolyhedraDatabase cannot be null")
  with_mock(
       "Rpolyhedra::getUserSpace" = function(){
            file.path(getUserSpace(), ".tmp/")
         },
       testthat::expect(
       with_mock(
         "Rpolyhedra::getDataEnv" = function(){
           "HOME"
           }
         ,
         with_mock(
           "Rpolyhedra::checkDatabaseVersion" = function(){
             "UPDATE"
             }
           ,
           downloadRPolyhedraSupportingFiles() %in%
                  c("SUCCESS", "NOT_AVAILABLE")
     )), failure_message = "downloadRPolyhedraSupportingFiles error"))

   with_mock(
     "Rpolyhedra::getDataDir" = function() {
       file.path(getDataDir(), ".tmp/")
      },
    testthat::expect_error(copyFilesToExtData(FALSE))
    )


  testthat::expect(!is.null(getPackageVersion()))
  testthat::expect(!is.null(getPackageDB()))
  testthat::expect(!is.null(getDatabaseVersion()))

  testthat::expect(switchToFullDatabase(env = "PACKAGE") == "PACKAGE")
})
