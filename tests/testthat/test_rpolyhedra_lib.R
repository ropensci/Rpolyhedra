
# Scrape tests
test_that("Scrape test rpolyhedra 5 regular solids", {
    scrapePolyhedra(max.quant = 5, home.dir.data = getDataDir(),
        test = TRUE)
})

test_that("Scrape test rpolyhedra all polyhedra", {
  scrapePolyhedra(home.dir.data = getDataDir(),
                  test = TRUE)
})

