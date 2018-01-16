context("test-coords2country")

test_that("Checks if the country is found right", {

  lat <- 33.40
  long <- 73.30
  country_res <- "PAKISTAN"
  country <- coords2country(lat, long)

  expect_equal(country, country_res)
})


