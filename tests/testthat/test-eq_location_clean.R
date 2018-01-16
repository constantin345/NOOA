context("eq_location_clean")

test_that("Checks if the LOCATION_NAME is cleaned.", {

  country <- c("JORDAN", "TURKMENISTAN", "SYRIA", "GREECE", "ISRAEL", "ITALY",
               "SYRIA", "ISRAEL", "JORDAN", "ISRAEL")


  loc_name <- c("JORDAN:  BAB-A-DARAA,AL-KARAK", "TURKMENISTAN:  W", "SYRIA:  UGARIT",
                "GREECE:  THERA ISLAND (SANTORINI)", "ISRAEL:  ARIHA (JERICHO)",
                "ITALY:  LACUS CIMINI", "SYRIAN COASTS", "ISRAEL:  ARIHA (JERICHO)",
                "JORDAN:  SW:  TIMNA COPPER MINES", "ISRAEL:  JERUSALEM")


  loc_name_res<- c("Bab-A-Daraa,Al-Karak", "W", "Ugarit", "Thera Island (Santorini)",
                   "Ariha (Jericho)", "Lacus Cimini", "Syrian Coasts", "Ariha (Jericho)",
                   "Timna Copper Mines", "Jerusalem")

  df <- data.frame(COUNTRY=country, LOCATION_NAME=loc_name, stringsAsFactors = FALSE)
  df_res <- data.frame(COUNTRY=country, LOCATION_NAME=loc_name_res, stringsAsFactors = FALSE)

  expect_equal(eq_location_clean(df), df_res)
})


