# Scrape tests
context("ledger")
test_that("create minimal ledger", {
  ledger <- ScraperLedger.class$new()
  #initialize variables
  source.config.netlib <- getPackageEnvir(".available.sources")[["netlib"]]

  scrapeInLedger <- function(ledger, source.config, filename){
    #Simulates scrape desired behavior
    polyhedra.dir <- source.config$getBaseDir(getPackageDir())
    source <- source.config$getName()
    ledger$addFilename(source = source, source.filename = filename)
    ledger$updateStatus(source = source,
                        source.filename = filename,
                        status = "scraping")
    scraped.polyhedron <- source.config$scrape(
      polyhedron.file.id = filename,
      file.path(polyhedra.dir,
                filename))
    ledger$updateStatus(source = source,
                        source.filename = filename,
                        status = "scraped",
                        scraped.polyhedron = scraped.polyhedron)
  }
  #Fill ledger
  scrapeInLedger(ledger = ledger, source.config = source.config.netlib, filename = "0")
  scrapeInLedger(ledger = ledger, source.config = source.config.netlib, filename = "1")
  scrapeInLedger(ledger = ledger, source.config = source.config.netlib, filename = "2")

  # Execute functions
  ledger.crc   <-  ledger$getCRCPolyhedronName(source = "netlib", polyhedron.name = "tetrahedron")
  expect_equal(ledger$getAvailableSources(), "netlib")
  expect_equal(ledger$getAvailablePolyhedra()$scraped.name, c("tetrahedron", "octahedron", "cube"))
  expect_equal(unique(ledger$getFilenamesStatus(status = "scraped")$status), "scraped")
  expect_equal(unique(ledger$getFilenamesStatusMode(mode = "test")$status), "scraped")

  ledger$countStatusUse(status.field = "status", status = "scraped")

  })

context("db")
test_that("create minimal db", {
  expect_equal(selectDataEnv("PACKAGE"), "PACKAGE")
  source.config.netlib <- PolyhedronScraperConfigurationNetlib.class$new()
  source.config.dmccooey <- PolyhedronScraperConfigurationDmccoey.class$new()
  expect_equal(source.config.netlib$getBaseDir("destdir"), "destdir/sources/www.netlib.org/polyhedra/")
  expect_equal(source.config.dmccooey$getBaseDir("destdir"), "destdir/sources/dmccooey.com/polyhedra/")

  db <- PolyhedraDatabase.class$new()
  #FIXME for building testcase: cannot exist polyhedron if it is not in the database.
  db$existsPolyhedron(source = "netlib", polyhedron.name = "tetrahedron")
  db$getPolyhedronFilename(source = "netlib", polyhedron.name = "tetrahedron",
                        extension = ".RDS.zip")
  #test sources
  db$addSourceConfig(source.config = source.config.netlib)
  db$configPolyhedraSource(source.config = source.config.netlib, source.filenames = as.character(0:10))
  db$scrape(mode = "scrape.queued",sources = "netlib",max.quant = 3)
  expect_equal(db$getAvailablePolyhedra()$scraped.name, c("tetrahedron", "octahedron", "cube"))
})

context("Regular solids")
test_that("Scrape test rpolyhedra 5 regular solids", {
  tasks <- getPolyhedraObject()$generateTestTasks(sources = "netlib",
      polyhedra.names = c("tetrahedron",
                          "octahedron",
                          "cube",
                          "dodecahedron",
                          "icosahedron"),
      TestTaskClass = PolyhedronTestTaskScrape.class,
                          max.quant = 5)
  for (task in tasks){
    task$run()
  }
})

context("Scrape available polyhedra")
test_that("Scrape test rpolyhedra for 12% of available polyhedra", {
  tasks <- getPolyhedraObject()$
    generateTestTasks(TestTaskClass = PolyhedronTestTaskScrape.class,
    max.quant = getPercentilPolyhedraQuant(0.12, 50))
  for (task in tasks){
    task$run()
  }
})

context("Properties regular solids")
test_that("test getting properties of scraped regular solids", {
  tetrahedron <- getPolyhedron(source = "netlib",
                               polyhedron.name = "tetrahedron")
  tetrahedron$checkProperties(expected.vertices = 4,
                              expected.faces = 4)

  octahedron <- getPolyhedron(source = "netlib",
                              polyhedron.name = "octahedron")
  octahedron$checkProperties(expected.vertices = 6,
                             expected.faces = 8)
  cube <- getPolyhedron(source = "netlib",
                        polyhedron.name = "cube")
  cube$checkProperties(expected.vertices = 8,
                       expected.faces = 6)

  icosahedron <- getPolyhedron(source = "netlib",
                               polyhedron.name = "icosahedron")
  icosahedron$checkProperties(expected.vertices = 12,
                              expected.faces = 20)
  dodecahedron <- getPolyhedron(source = "netlib",
                                polyhedron.name = "dodecahedron")
  dodecahedron$checkProperties(expected.vertices = 20,
                               expected.faces = 12)
})


context("Edges Consistency")
test_that(paste("test check edges consistency for 12% of",
            "available polyhedra"), {
  tasks <- getPolyhedraObject()$
    generateTestTasks(TestTaskClass = PolyhedronTestTaskEdgesConsistency.class,
                    max.quant = getPercentilPolyhedraQuant(0.12, 50))

  for (task in tasks){
    task$run()
  }
})

context("XML export")
test_that("test xml can be exported", {
  xml.polyhedron <- polyhedronToXML(getPolyhedron("netlib", "tetrahedron")$getState())
  expect_equal(as.numeric(nchar(XML::getChildrenStrings(xml.polyhedron))), 538)
  })
