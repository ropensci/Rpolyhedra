context("serialization-lib XML export")
test_that("test xml can be exported", {
  xml.polyhedron <- polyhedronToXML(
          getPolyhedron("netlib", "tetrahedron")$getState())
  test_that::expect_equal(
    as.numeric(nchar(XML::getChildrenStrings(xml.polyhedron))), 406)
})
