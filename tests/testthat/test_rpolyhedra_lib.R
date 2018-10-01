# Scrape tests
context("Regular solids")
test_that("Scrape test rpolyhedra 5 regular solids", {
  tasks <- getPolyhedraObject()$generateTestTasks(sources = "netlib",
                                                  polyhedra.names = c("tetrahedron","octahedron","cube","dodecahedron","icosahedron"),
                                                  TestTaskClass = PolyhedronTestTaskScrape.class,
                                                  max.quant = 5)
  for (task in tasks){
    task$run()
  }
})

context("Scrape available polyhedra")
test_that("Scrape test rpolyhedra for 12% of available polyhedra", {
  tasks <- getPolyhedraObject()$generateTestTasks(TestTaskClass = PolyhedronTestTaskScrape.class,
                                        max.quant = getPercentilPolyhedraQuant(0.12,50))

  for (task in tasks){
    task$run()
  }
})

context("Properties regular solids")
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
test_that("test check edges consistency for 7% of available polyhedra", {
  tasks <- getPolyhedraObject()$generateTestTasks(TestTaskClass = PolyhedronTestTaskEdgesConsistency.class,
                                        max.quant = getPercentilPolyhedraQuant(0.12,50))

  for (task in tasks){
    task$run()
  }
})
