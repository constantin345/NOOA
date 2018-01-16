context("test-fix_na_longitude")

test_that("Checks if  the cases with LONGITUDE is NA and LATITUDE is not NA, are corrected.", {

  lat <- 52
  long <- NA
  country <- "UK"
  loc <- "UNITED KINGDOM:  BRITIAN"
  year <- 1048
  month <- NA
  day <- NA
  id <- 338

  df <- data.frame(I_D=id, YEAR=year, MONTH=month,
                   DAY=day, COUNTRY=country, LOCATION_NAME=loc,
                   LATITUDE=lat, LONGITUDE=long, stringsAsFactors = FALSE)

  month_res <- 5
  day_res <- 1
  lat_res <- 52.19364
  long_res <-  -2.221575

  df_res <- data.frame(I_D=id, YEAR=year, MONTH=month_res,
                   DAY=day_res, COUNTRY=country, LOCATION_NAME=loc,
                   LATITUDE=lat_res, LONGITUDE=long_res, stringsAsFactors = FALSE)

  expect_equal(fix_na_longitude(df), df_res)
})


