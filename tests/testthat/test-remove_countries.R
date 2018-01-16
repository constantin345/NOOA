context("test-remove_countries")

test_that("Checks if removes from LOCATION_NAME the string which representing a name of country.", {

  country <- c("BULGARIA", "BULGARIA", "ROMANIA", "BULGARIA", "BULGARIA",
               "ROMANIA", "BULGARIA", "ROMANIA", "ROMANIA")


  loc_name <- c("BULGARIA:  BISONE [KAVARNA], DIONISOPOLIS [BALCHIK]", "BULGARIA",
                "ROMANIA", "BULGARIA", "BULGARIA;  PLOVDIV",
                "; ROMANIA:  CARPATHIAN FOLD,VRANCEA", "BULGARIA", "; ROMANIA", "ROMANIA C")

  loc_name_res<- c("BULGARIA:  BISONE [KAVARNA], DIONISOPOLIS [BALCHIK]", NA,
                    NA, NA, "  PLOVDIV", ":  CARPATHIAN FOLD,VRANCEA", NA, NA, "ROMANIA C")

  df <- data.frame(COUNTRY=country, LOCATION_NAME=loc_name, stringsAsFactors=FALSE)
  df_res <- data.frame(COUNTRY=country, LOCATION_NAME=loc_name_res, stringsAsFactors=FALSE)

  expect_equal(remove_countries(df), df_res)
})


