context("test-fix_countries")

test_that("Checks if the countries are fixed.", {

  filename <- "dfC.rds"
  filename_path <- system.file("extdata", filename, package="NOOA")
  dfC <- readRDS(filename_path)

  filename <- "dfCres.rds"
  filename_path <- system.file("extdata", filename, package="NOOA")
  dfCres <- readRDS(filename_path)

  expect_equal(fix_countries(dfC), dfCres)
})


