context("test-fix_na_latitude")

test_that("Checks if  the cases with LATITUDE is NA and LONGITUDE is not NA, are corrected.", {

  id <- c(6612L, 3567L, 3651L, 3705L, 3800L)
  year <- c(1885L, 1935L, 1939L, 1942L, 1945L)
  country <- c("INDONESIA", "INDONESIA", "INDONESIA", "INDONESIA", "UGANDA")
  loc <- c("INDONESIA:  SUMATRA:  AJERBANGIS", "INDONESIA:  N SUMATERA:  BATU I,PADANG,SIBOLGA",
           "INDONESIA:  CENTRTAL SULAWESI:  KALO,LUWUK,SULA I", "INDONESIA:  MINAHASSA PENINSULA",
           "UGANDA:  MASARA")
  lat <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  long <- c(99.5, 98.25, 123, 124, 32)

  df <- data.frame(I_D=id, YEAR=year, COUNTRY=country, LOCATION_NAME=loc,
                   LATITUDE=lat, LONGITUDE=long, stringsAsFactors = FALSE)

  loc_res <- c("INDONESIA:  SUMATRA:  AJERBANGIS",
               "INDONESIA:  N SUMATERA:  BATU I,PADANG,SIBOLGA",
               "Gulf of Tomini", "INDONESIA:  MINAHASSA PENINSULA",
               "Sembabule-Masaka")

  lat_res <- c(0.2, 0.001, -0.590336, 0.001, 0.233333)
  long_res <- c(99.38, 98.25, 122.092224, 124, 31.05)

  df_res <- data.frame(I_D=id, YEAR=year, COUNTRY=country, LOCATION_NAME=loc_res,
              LATITUDE=lat_res, LONGITUDE=long_res, stringsAsFactors = FALSE)

  expect_equal(fix_na_latitude(df), df_res)
})


