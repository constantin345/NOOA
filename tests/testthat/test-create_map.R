context("test-create_map")
test_that("Checks if the map with position of earthquakes is generated", {

  library(leaflet)
  library(dplyr)

  df <- load_data()
  df <- eq_clean_data(df)
  df <- eq_location_clean(df)

  dff <- filter(df, COUNTRY=="CHINA" & YEAR>=2010)
  magnitude <- ifelse(is.na(dff$EQ_PRIMARY),4,dff$EQ_PRIMARY)

  content <- paste("<b>Location:</b>", df$LOCATION_NAME,"<br/>",
   "<b>Magnitude:</b>", df$EQ_PRIMARY,"<br/>",
  "<b>Total deaths:</b>", df$TOTAL_DEATHS)

  a <- create_map(dff, content)

  filename <- "create_map_test.RDS"
  filename_path <- system.file("extdata", filename, package="NOOA")
  b <- readRDS(filename_path)

  expect_equal(a, b)
})


