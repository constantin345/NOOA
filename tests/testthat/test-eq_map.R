context("test-eq_map")
test_that("Checks if the map with earthquakes is generated", {

  library(lubridate)
  library(dplyr)

  dff <- load_data()
  dff <- eq_clean_data(dff)
  dff <- filter(dff, COUNTRY == "MEXICO" & lubridate::year(DataEQ) >= 2000)

  a <- eq_map(dff, annot_col = "DataEQ")

  filename <- "eq_map_test.RDS"
  filename_path <- system.file("extdata", filename, package="NOOA")
  b <- readRDS(filename_path)

  expect_equal(a, b)
})


