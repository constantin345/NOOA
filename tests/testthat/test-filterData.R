context("test-filterData")
test_that("Checks if the filterData function works.", {

  df <- load_data()
  df <- eq_clean_data(df)
  df <- eq_location_clean(df)

  dfFilter <- filterData(df, COUNTRY="CUBA", minYear=1880, maxYear=1992)

  df_res <- data.frame(YEAR=c(1880L, 1932L, 1939L, 1992L),
                       COUNTRY=c("CUBA", "CUBA", "CUBA", "CUBA"),
                       LOCATION_NAME=c("San Cristobal,Candelaria",
                                       "Santiago De Cuba", "Santa Clara",
                                       "Pilon, Manzanillo"),
                       EQ_PRIMARY=c(1, 5.4, 5.6, 7),
                       DEATHS=c(NA, 8, NA, NA),
                       DataEQ=structure(c(-32850, -13847, -11097, 8180),
                                        class = "Date"),
                       stringsAsFactors = FALSE)


  expect_equal(dfFilter, df_res)
})


