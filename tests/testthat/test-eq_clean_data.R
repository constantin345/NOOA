context("test-eq_clean_data")

test_that("Checks if the function eq_clean_data works.", {
  filename <- "dfclean.rds"
  filename_path <- system.file("extdata", filename, package="NOOA")
  dfclean <- readRDS(filename_path)

  filename <- "dfclean_res.rds"
  filename_path <- system.file("extdata", filename, package="NOOA")
  dfclean_res <- readRDS(filename_path)

  expect_equal(eq_clean_data(dfclean), dfclean_res)
})


