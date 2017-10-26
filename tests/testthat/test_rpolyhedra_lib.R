
# Scrape tests
test_that("Scrape test rpolyhedra 5 regular solids", {
    scrapePolyhedra(max.quant = 5, test = TRUE)
})

test_that("Scrape test rpolyhedra all polyhedra", {
  scrapePolyhedra(test = TRUE)
})

