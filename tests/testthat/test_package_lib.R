
context("package-lib")

setup(tmp.package.dir <<-
          file.path(getDataDir(data.env = "PACKAGE"), ".tmp/"))
setup(tmp.home.dir <<- file.path(getUserSpace(), ".tmp/"))


test_that("test on package lib functions", {
  testthat::expect(!is.null(getPreloadedDataFilename()),
                   failure_message = "getPreloadedDataFilename cannot be null")
  testthat::expect(!is.null(updatePolyhedraDatabase()),
                   failure_message = "updatePolyhedraDatabase cannot be null")
  with_mock(
       "Rpolyhedra::getUserSpace" = function(){
            tmp.home.dir
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

  testthat::expect_equal(copyFilesToExtData(force = FALSE,
                                            source.folder =
                                              getDataDir(data.env =  "HOME"),
                                            dest.folder = tmp.package.dir),
                                                   TRUE)


  testthat::expect(!is.null(getPackageVersion()))
  testthat::expect(!is.null(getPackageDB()))
  testthat::expect(!is.null(getDatabaseVersion()))

  testthat::expect(switchToFullDatabase(env = "PACKAGE") == "PACKAGE")
})

teardown(unlink(tmp.package.dir, recursive = TRUE))
teardown(unlink(tmp.home.dir, recursive = TRUE))

