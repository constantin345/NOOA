context("test-convert_data")

test_that("Joining three vectors (Year, Month, Day) creates a Date vector", {
  Year <- c(-11, -100, -200, -300, 2000, 0)
  Month <- c(NA, NA, NA, 9, 8, 1)
  Day <- c(8, NA, 7, 29, 20, 12)
  dd<-convert_data(Year, Month, Day)
  result <- c("-011-01-08","-100-01-01","-200-01-07","-300-09-29" ,"2000-08-20", "0000-01-12")

  m <- strsplit(result,"-")
  monthres <- lapply(m, function(x) x[x!=""][2])
  Month_res <- unlist(as.numeric(monthres))
  Month_res[is.na(Month_res)] <- 1
  Month[is.na(Month)] <- 1

  y <- gsub("-[0-9][0-9]-[0-9][0-9]","",result)
  Year_res <- as.numeric(y)


  dayres <- lapply(m, function(x) x[x!=""][3])
  Day_res <- unlist(as.numeric(dayres))
  Day_res[is.na(Day_res)] <- 1
  Day[is.na(Day)] <- 1

  expect_equal(class(dd), "Date")
  expect_equal(Month, Month_res)
  expect_equal(Day, Day_res)
  expect_equal(Year, Year_res)
})

test_that("Throws an error if Year, Month and Day don't have the same length", {
  Year <- c(-11, -100, -200, -300)
  Month <- c(NA, NA, NA, 9, 8, 1)
  Day <- c(8, NA, 7, 29, 20, 12)
  err <- "Year, Month and Day have to have the same length!"
  expect_error(convert_data(Year, Month, Day), err)
})
