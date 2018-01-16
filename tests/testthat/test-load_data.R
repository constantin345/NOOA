context("test-load_data")

test_that("Checks if  the file with earthquakes is loaded correct in memory.", {
  df <- load_data()
  filename<-system.file("extdata","signif.txt",package="NOOA")
  df_res <- readr::read_delim(filename, delim  = "\t")
  expect_equal(df, df_res)
})


