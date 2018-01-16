context("test-geom_timeline_label")
test_that("Checks if geom_timeline_label extension is correct generated", {

  library(ggplot2)
  df <- load_data()
  df <- eq_clean_data(df)
  df <- eq_location_clean(df)

  country <- c("JAPAN")
  dfFilter <- filterData(df, COUNTRY=country, minYear=1000, maxYear=1400)

  test <- (ggplot()+
             geom_timeline(data=dfFilter,aes(x=DataEQ,
                                       xmin=as.Date("1000-01-01"),
                                       xmax=as.Date("1400-12-31"),
                                       colour=DEATHS,
                                       fill=DEATHS,
                                       size=EQ_PRIMARY,
                                       y=COUNTRY))+
             geom_timeline_label(data=dfFilter, aes(x=DataEQ,
                                              location=LOCATION_NAME,
                                              size=EQ_PRIMARY,
                                              n_max=5,
                                              y=COUNTRY)))

  a <- setupGeom(test)
  expect_true(is(a, 'ggplot'))

})


