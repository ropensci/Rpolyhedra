
context("db-lib")
test_that("create minimal db", {
  initDataDirEnvironment()
  expect_equal(selectDataEnv("PACKAGE"), "SUCCESS")
  source.config.netlib <- PolyhedronScraperConfigurationNetlib.class$new()
  source.config.dmccooey <- PolyhedronScraperConfigurationDmccoey.class$new()
  expect_equal(source.config.netlib$getBaseDir("destdir"),
               "destdir/sources/www.netlib.org/polyhedra/")
  expect_equal(source.config.dmccooey$getBaseDir("destdir"),
               "destdir/sources/dmccooey.com/polyhedra/")

  db <- PolyhedraDatabase.class$new()
  #FIXME for building testcase: cannot exist polyhedron if it is not in the database.
  db$existsPolyhedron(source = "netlib", polyhedron.name = "tetrahedron")
  db$getPolyhedronFilename(source = "netlib", polyhedron.name = "tetrahedron",
                           extension = ".RDS.zip")
  #test sources
  db$addSourceConfig(source.config = source.config.netlib)
  db$addSourceConfig(source.config = source.config.dmccooey)
  db$configPolyhedraSource(source.config = source.config.netlib,
                           source.filenames = NULL)
  db$configPolyhedraSource(source.config = source.config.dmccooey,
                           source.filenames = NULL)
  db$scrape(mode = "scrape.queued",sources = "netlib",max.quant = 3,
            skip.still.queued = FALSE)
  db$scrape(mode = "scrape.queued",sources = "dmccooey",max.quant = 3,
            skip.still.queued = TRUE)
  expect_equal(db$getAvailablePolyhedra()$scraped.name,
               c("tetrahedron", "octahedron", "cube",
                 "10-truncated triakis icosahedron (canonical)",
    "4-5-truncated deltoidal hexecontahedron with truncation depth chosen",
                 "132-pentagon polyhedron"
               ))
  #TODO understand why generateTestTasks doesn't get covered by covr package
  tasks <- getPolyhedraObject()$
    generateTestTasks(TestTaskClass = PolyhedronTestTaskScrape.class,
                      max.quant = 3)
  expect_equal(length(tasks), 3)
  #TODO
  #testRR

})

