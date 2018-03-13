context("Rpolyhedra-test")

# Scrape tests
context("regular solids")
test_that("Scrape test rpolyhedra 5 regular solids", {
  tasks <- .polyhedra$generateTestTasks(sources = "netlib",TestTaskClass = PolyhedronTestTaskScrape.class,
                                        max.quant = 5)
  for (task in tasks){
    task$run()
  }
})

context("scrape all polyhedra")
test_that("Scrape test rpolyhedra all polyhedra", {
  tasks <- .polyhedra$generateTestTasks(TestTaskClass = PolyhedronTestTaskScrape.class,
                                        max.quant = 0)

  for (task in tasks){
    task$run()
  }
})

context("properties regular solids")
test_that("test getting properties of scraped regular solids", {
  tetrahedron <- getPolyhedron(source = "netlib",polyhedron.name = "tetrahedron")
  tetrahedron$checkProperties(expected.vertices = 4, expected.faces = 4)

  octahedron <- getPolyhedron(source = "netlib",polyhedron.name = "octahedron")
  octahedron$checkProperties(expected.vertices = 6, expected.faces = 8)
  cube <- getPolyhedron(source = "netlib",polyhedron.name = "cube")
  cube$checkProperties(expected.vertices = 8, expected.faces = 6)

  icosahedron <- getPolyhedron(source = "netlib",polyhedron.name = "icosahedron")
  icosahedron$checkProperties(expected.vertices = 12, expected.faces = 20)
  dodecahedron <- getPolyhedron(source = "netlib",polyhedron.name = "dodecahedron")
  dodecahedron$checkProperties(expected.vertices = 20, expected.faces = 12)
})


context("Edges Consistency")
test_that("test check edges consistency for all polyhedra", {
  tasks <- .polyhedra$generateTestTasks(TestTaskClass = PolyhedronTestTaskEdgesConsistency.class,
                                        max.quant = 0)

  for (task in tasks){
    task$run()
  }
})

context("export XML")
test_that("an XML document can be created out of a cube (netlib)", {
  cube <- getPolyhedron("netlib", "cube")
  cube.xml <- cube$exportToXML()
  cube.str <- XML::saveXML(cube.xml)
  expect(is.null(cube.str) == FALSE, "The document could not be saved")
})
