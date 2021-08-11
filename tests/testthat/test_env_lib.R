context("env-lib")
testthat::test_that("test on env lib functions", {

  setUserEnvir("testUser", "testUser")
  testthat::expect_equal(getUserEnvir(variable.name = "testUser"), "testUser")
  testthat::expect_equal(setDataDirEnvironment(env = "PACKAGE"), "PACKAGE")
  testthat::expect_equal(getDataEnv(), "PACKAGE")

  testthat::expect(!is.null(getUserSpace()),
                   failure_message =   "getUserSpace cannot be null")
  testthat::expect_equal(initDataDirEnvironment(), "PACKAGE")
  testthat::expect(!is.null(getDataDir()),
                   failure_message = "getDataDir cannot be null")
  testthat::expect(!is.null(getEnvironmentFilepath()),
                   failure_message = "getEnvironmentFilepath cannot be null")
  testthat::expect(!is.null(getPackageDir()),
                   failure_message = "getPackageDir cannot be null")
  testthat::expect(!is.null(getPolyhedraRDSPath()),
                   failure_message = "getPolyhedraRDSPath cannot be null")
  testthat::expect_equal(selectDataEnv(env = "PACKAGE"), "PACKAGE")
  testthat::expect_equal(selectDataEnv(prompt.value = "n"), "PACKAGE")


  #selectDataEnv(env=NA) cannot be tested because prompt
  testthat::expect(!is.null(getPolyhedraObject()),
                   failure_message = "getPolyhedraObject cannot be null")
  testthat::expect(!is.null(checkDatabaseVersion()),
                   failure_message="Database Version cannot be null")
  testthat::expect(isCompatiblePolyhedraRDS() == TRUE,
                   failure_message="DB has to be compatible")
  testthat::expect(!is.null(scrapePolyhedraSources()),
                   failure_message="Polyhedra sources must be able to be scrapped")
  testthat::expect(!is.null(getGitCommit()),
                   failure_message="Git commit cannot be null")
})
