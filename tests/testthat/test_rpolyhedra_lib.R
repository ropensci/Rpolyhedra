# Scrape tests
context("ledger")
test_that("create minimal ledger", {
  ledger <- ScraperLedger.class$new()
  cube <- getPolyhedron(source = "netlib", polyhedron.name = "cube")
  cube$state$name = "cube_test"
  #we copy over the database's df as to be able to test something
  ledger$df <- getPolyhedraObject()$ledger$df
  ledger1 <- ledger$clone()
  ledger$resetStatesMetrics()
  ledger$updateCalculatedFields()

  ledger.count <- nrow(ledger$loadPreloadedData())
  ledger.crc = ledger$getCRCPolyhedronName(source = "netlib", polyhedron.name = "cube")
  ledger$getAvailableSources()
  ledger$getAvailablePolyhedra()
  ledger$getFilenamesStatus("scraped")
  ledger$countStatusUse(status.field = "status", status = "scraped")
  ledger$getFilenamesStatusMode(mode="test")

  ledger$getIdFilename(source = "netlib", source.filename = "0")
  ledger$addFilename(source = "netlib", source.filename = "1001")
  ledger$updateStatus(source = "netlib", source.filename = "1001",
        status = "scraped", status.field = "status",
        scraped.polyhedron = cube,
        obs = "just a test")
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
  polyhedronToXML(getPolyhedron("netlib", "cube")$getState())
  })
