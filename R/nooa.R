#' convert_data
#'
#' @description A date vector is created by uniting the year, month, day and
#' converting it to the Date class
#'
#' @param Year An integer vector representing years
#' @param Month An integer vector representing months
#' @param Day An integer vector representing days
#' @return A date vector or an error message if the input parameters
#' don't have the same length
#'
#' @export
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#'
#' @examples
#' Year <- c(-11, -100, -200, -300, 2000, 0)
#' Month <- c(NA, NA, NA, 9, 8, 1)
#' Day <- c(8, NA, 7, 29, 20, 12)
#'
#' convert_data(Year, Month, Day)
#'
convert_data <- function(Year=0, Month=0, Day=0)
{
  check_ <- length(Year)!=length(Month)
  check_ <- check_ | length(Year)!=length(Day)
  check_ <- check_ | length(Day)!=length(Month)

  if(check_)
  {
    stop("Year, Month and Day have to have the same length!")
  }

  Month[is.na(Month)] <- 1
  Day[is.na(Day)] <- 1

  d <- lubridate::as_date("1970-01-01")

  lubridate::year(d) <- Year
  lubridate::month(d) <- Month
  lubridate::day(d) <- Day

  d
}

#' load_data
#'
#' Load in a dataframe Earthquate data obtained from
#' https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#' @return A date frame
#' @export
#' @importFrom readr read_delim
#'
#' @examples
#' df <- load_data()
#'
load_data <- function()
{
  filename<-system.file("extdata","signif.txt",package="NOOA")
  df <- suppressMessages(readr::read_delim(filename, delim="\t"))
  df
}

#' remove_countries
#'
#' @description Removes from column LOCATION_NAME (a NOAA dataframe) the strings
#' which representing a name of country.
#'
#' @param df A dataframe containing NOAA Earthquake data
#' @return A dataframe with column LOCATION_NAME modified
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#'
#' @examples
#' dfnew <- load_data()
#' dff <- remove_countries(dfnew)
#'
remove_countries <- function(df)
{
  countryPrep <- df$COUNTRY %>% unique %>%
    stringr::str_replace_all("\\(","\\\\(") %>%
    stringr::str_replace_all("\\)","\\\\)")

  #countryPrep %>%
  pattern1 <- paste0("^",countryPrep,"$") %>%
              paste(collapse="|")

  #countryPrep %>%
  pattern2 <- paste0("^",countryPrep,";") %>%
              paste(collapse="|")

  #countryPrep %>%
  pattern3 <- paste0("; ",countryPrep) %>%
              paste(collapse="|")

  pattern <- paste(pattern1, pattern2, pattern3, sep="|")

  df$LOCATION_NAME <- stringr::str_replace_all(df$LOCATION_NAME, pattern, "")
  df$LOCATION_NAME[df$LOCATION_NAME==""] <- NA

  df
}

#' eq_location_clean
#'
#' @description Cleans the LOCATION_NAME column by stripping out
#' the country name (including the colon) and converts names to
#' title case (as opposed to all caps).
#'
#' @param df A dataframe containing NOAA Earthquake data
#' @return A dataframe with column LOCATION_NAME modified
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#'
#' @examples
#' dfnew <- load_data()
#' dff <- eq_location_clean(dfnew)
#'
eq_location_clean <- function(df)
{
  LOCATION_NAME <- NULL
  COUNTRY <- NULL

  df <-df %>%
    dplyr::mutate(LOCATION_NAME=
             stringr::str_replace_all(LOCATION_NAME, "[A-Z][A-Z,a-z,\\-,\\s]*:+","") %>%
             stringr::str_replace_all(paste0(COUNTRY,";"),"") %>%
             stringr::str_replace_all(paste0(",\\s+",COUNTRY),"") %>%
             stringr::str_replace_all(paste0("^",COUNTRY,"$"),"") %>%
             stringr::str_replace_all(paste0("[-,]",COUNTRY),"") %>%
             stringr::str_replace_all(paste0(COUNTRY,"[-,]"),""))  %>%
    dplyr::mutate(LOCATION_NAME=stringr::str_trim(LOCATION_NAME)) %>%
    remove_countries

  df$LOCATION_NAME <- stringr::str_to_title(df$LOCATION_NAME)
  df

}


#' fix_na_latitude
#'
#' @description Correct the cases with LATITUDE is NA and LONGITUDE is not NA.
#'
#' @details Information about those earthquakes was found in the next documents (sites):
#' \itemize{
#' \item \url{https://www.researchgate.net/publication/296476641_A_catalogue_of_earthquakes_between_810BC_and_2012_for_the_Bay_of_Bengal}
#' \item \url{http://www.unesco-uganda.ug/files/downloads/Ethquakes\%20in\%20Uganda.pdf} (see pg.3)
#' \item \url{https://en.wikipedia.org/wiki/List_of_earthquakes_in_1939}
#' }
#'
#' @param dff A dataframe containing NOAA Earthquake data
#' @return A date frame (with those cases corrected)
#' @export
#'
#' @examples
#'
#' dfnew <- load_data()
#' dfnew <- dplyr::mutate(dfnew, LATITUDE=as.numeric(LATITUDE))
#' dfnew <- dplyr::mutate(dfnew, LONGITUDE=as.numeric(LONGITUDE))
#' dfnew <- dplyr::mutate(dfnew, EQ_PRIMARY=as.numeric(EQ_PRIMARY))
#' dfnew <- dplyr::mutate(dfnew, DEATHS=as.numeric(DEATHS))
#' dfnew <- fix_na_latitude(dfnew)
#'
fix_na_latitude <- function(dff)
{
  cond <- is.na(dff$LATITUDE) & !is.na(dff$LONGITUDE)

  condCountry <- dff$YEAR==1885 & dff$COUNTRY=="INDONESIA"
  ##https://www.researchgate.net/publication/296476641_A_catalogue_of_earthquakes_between_810BC_and_2012_for_the_Bay_of_Bengal
  dff$LATITUDE[cond & condCountry] <- 0.2
  dff$LONGITUDE[cond & condCountry] <- 99.38
  id <- dff$I_D[cond & condCountry]
  idd <- c(id)


  ##http://www.unesco-uganda.ug/files/downloads/Ethquakes%20in%20Uganda.pdf
  ##pg. 3

  condCountry <- dff$COUNTRY=="UGANDA"
  dff$LOCATION_NAME[cond & condCountry] <- "Sembabule-Masaka"
  dff$LATITUDE[cond & condCountry] <- 0.233333
  dff$LONGITUDE[cond & condCountry] <- 31.05
  id <- dff$I_D[cond & condCountry]
  idd <- c(id, idd)


  #aprox latitude. I didn't find any information about the latitude
  condCountry <- dff$YEAR==1942 & dff$COUNTRY=="INDONESIA"
  dff$LATITUDE[cond & condCountry] <- 0.001
  id <- dff$I_D[cond & condCountry]
  idd <- c(id, idd)


  ##https://www.researchgate.net/publication/296476641_A_catalogue_of_earthquakes_between_810BC_and_2012_for_the_Bay_of_Bengal
  condCountry <- dff$YEAR==1935 & dff$COUNTRY=="INDONESIA"
  dff$LATITUDE[cond & condCountry] <- 0.001
  id <- dff$I_D[cond & condCountry]
  idd <- c(id, idd)


  #https://en.wikipedia.org/wiki/List_of_earthquakes_in_1939
  condCountry <- dff$YEAR==1939 & dff$COUNTRY=="INDONESIA"
  dff$LATITUDE[cond & condCountry] <- -0.590336
  dff$LONGITUDE[cond & condCountry] <-  122.092224
  dff$LOCATION_NAME[cond & condCountry] <- "Gulf of Tomini"
  id <- dff$I_D[cond & condCountry]
  idd <- c(id, idd)

  dff

}


#' fix_na_longitude
#'
#' @description Correct the cases with LATITUDE is not NA and LONGITUDE is NA.
#'
#' @details Information about those earthquakes was found in the next documents (sites):
#' \itemize{
#' \item \url{https://en.wikipedia.org/wiki/List_of_earthquakes_in_the_British_Isles}
#' }
#'
#' @param dff A dataframe containing NOAA Earthquake data
#' @return A date frame (with those cases corrected)
#' @export
#'
#' @examples
#' dfnew <- load_data()
#' dfnew <- dplyr::mutate(dfnew, LATITUDE=as.numeric(LATITUDE))
#' dfnew <- dplyr::mutate(dfnew, LONGITUDE=as.numeric(LONGITUDE))
#' dfnew <- dplyr::mutate(dfnew, EQ_PRIMARY=as.numeric(EQ_PRIMARY))
#' dfnew <- dplyr::mutate(dfnew, DEATHS=as.numeric(DEATHS))
#' dfnew <- fix_na_longitude(dfnew)
#'
fix_na_longitude <- function(dff)
{
  #https://en.wikipedia.org/wiki/List_of_earthquakes_in_the_British_Isles
  #just I put location of Worcester UK
  cond <- !is.na(dff$LATITUDE) & is.na(dff$LONGITUDE)
  condCountry <- dff$YEAR==1048 & dff$COUNTRY=="UK"

  dff$MONTH[cond & condCountry] <- 5
  dff$DAY[cond & condCountry] <- 1
  dff$LATITUDE[cond & condCountry] <- 52.19364
  dff$LONGITUDE[cond & condCountry] <-  -2.221575
  id <- dff$I_D[cond & condCountry]

  dff
}


#' fix_na_lon_na_lat
#'
#' @description Correct the cases with LATITUDE is NA and LONGITUDE is NA.
#'
#' @details
#' Information about the location of those earthquakes was found using the
#' geocode function from ggmap package. The dataframe generated by
#' the geocode function was saved in the file "nulllatlong.rds".
#' For the cases where the geocode function could not find the location of the earthquakes,
#' the following documents were used (sites):
#' \itemize{
#' \item \url{ftp://ftp.ngdc.noaa.gov/hazards/publications/Wdcse-49.pdf} (pg. 248)
#' \item \url{https://mapcarta.com/32906862}
#' \item \url{http://www.jonfr.com/volcano/?p=745}
#' \item \url{https://mapcarta.com/19182696}
#' \item \url{http://www.pmd.gov.pk/Islamabadreport.pdf} (pg 22)
#' \item \url{https://arxiv.org/ftp/arxiv/papers/1708/1708.07262.pdf} (pg. 4)
#' \item \url{https://celt.ucc.ie/published/T100010A/text006.html}
#' }
#'
#' @param dff A dataframe containing NOAA Earthquake data
#' @return A date frame (with those cases corrected)
#' @export
#'
#' @examples
#' dfnew <- load_data()
#' dfnew <- dplyr::mutate(dfnew, LATITUDE=as.numeric(LATITUDE))
#' dfnew <- dplyr::mutate(dfnew, LONGITUDE=as.numeric(LONGITUDE))
#' dfnew <- dplyr::mutate(dfnew, EQ_PRIMARY=as.numeric(EQ_PRIMARY))
#' dfnew <- dplyr::mutate(dfnew, DEATHS=as.numeric(DEATHS))
#' dfnew <- fix_na_lon_na_lat(dfnew)
#'
fix_na_lon_na_lat <- function(dff)
{
  cond <- is.na(dff$LATITUDE) & is.na(dff$LONGITUDE)
  linii <- which(cond)

  #the file was generate using geocode function
  filename<-system.file("extdata","nulllatlong.rds",package="NOOA")
  poz <- readRDS(filename)
  dff$LONGITUDE[cond] <- poz$lon
  dff$LATITUDE[cond] <- poz$lat

  #see ftp://ftp.ngdc.noaa.gov/hazards/publications/Wdcse-49.pdf (p. 248)
  condCountry <- dff$COUNTRY=="TURKEY" & dff$YEAR==1269
  dff$LATITUDE[cond & condCountry] <- 37.5
  dff$LONGITUDE[cond & condCountry] <- 35.5

  #cond <- dff$I_D %in% c(7263, 7264)
  ##see https://mapcarta.com/32906862 and http://www.jonfr.com/volcano/?p=745
  condCountry <- dff$COUNTRY=="ICELAND" & dff$YEAR==1732
  dff$LATITUDE[cond & condCountry] <- 63.8417
  dff$LONGITUDE[cond & condCountry] <- -20.2554
  dff$LOCATION_NAME[cond & condCountry] <- "Rangarvellir"
  dff$MONTH[cond & condCountry] <- 9
  dff$DAY[cond & condCountry] <- 7

  ##see  https://mapcarta.com/19182696 and http://www.jonfr.com/volcano/?p=745
  condCountry <- dff$COUNTRY=="ICELAND" & dff$YEAR==1734
  dff$LATITUDE[cond & condCountry] <- 64.25
  dff$LONGITUDE[cond & condCountry] <- -20.5
  dff$LOCATION_NAME[cond & condCountry] <- "Arnessysla"
  dff$MONTH[cond & condCountry] <- 3
  dff$DAY[cond & condCountry] <- 21

  #see http://www.pmd.gov.pk/Islamabadreport.pdf (pg 22)
  #see https://arxiv.org/ftp/arxiv/papers/1708/1708.07262.pdf (pg. 4)
  condCountry <- dff$COUNTRY=="INDIA" & dff$YEAR==1669
  id <- dff$I_D[cond & condCountry]
  dff$LOCATION_NAME[dff$I_D==id] <- "Mandra"
  dff$COUNTRY[dff$I_D==id] <- "PAKISTAN"
  dff$LATITUDE[dff$I_D==id] <- 33.40
  dff$LONGITUDE[dff$I_D==id] <- 73.30


  #https://celt.ucc.ie/published/T100010A/text006.html
  #I put the location of Northern Ireland
  condCountry <- dff$COUNTRY=="UK" & dff$YEAR==1118
  dff$LATITUDE[cond & condCountry] <- 54.78771
  dff$LONGITUDE[cond & condCountry] <- -6.492314

  dff
}


#' coords2country
#'
#' @description  Finds the country where the location is located
#' by \code{latitude} and \code{longitude}.
#'
#' @details I made small modification to the function \strong{coords2continent}. See the location:
#' \url{https://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r}
#' Thanks to the user \strong{Andy} for the function \strong{coords2continent}.
#'
#' @param latitude A numeric value representing latitude
#' @param longitude A numeric value representing longitude
#' @return a string representing the name of the Country. The NA is returned
#' if the coordinates (logitude and latitude) is not in a country.
#'
#' @export
#'
#' @importFrom sp SpatialPoints
#' @importFrom sp proj4string
#' @importFrom sp CRS
#' @importFrom sp over
#' @importFrom rworldmap getMap
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom dplyr %>%
#'
#' @examples
#' country <- coords2country(33.40,73.30)
#'
coords2country <- function(latitude, longitude)
{

  points <- data.frame(lon=longitude, lat=latitude)
  countriesSP <- getMap(resolution='low')

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP <- SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

  # use 'over' to get indices of the Polygons object containing each point
  indices <- over(pointsSP, countriesSP)

  country <- as.character(indices$ADMIN)


  country <- country %>%
    str_to_upper %>%
    str_replace_all("UNITED STATES OF AMERICA", "USA") %>%
    str_replace_all("UNITED STATES VIRGIN ISLANDS", "USA TERRITORY") %>%
    str_replace_all("UNITED KINGDOM", "UK") %>%
    str_replace_all("UNITED REPUBLIC OF TANZANIA", "TANZANIA") %>%
    str_replace_all("REPUBLIC OF SERBIA|MONTENEGRO", "SERBIA AND MONTENEGRO") %>%
    str_replace_all("BOSNIA AND HERZEGOVINA", "BOSNIA-HERZEGOVINA") %>%
    str_replace_all("MYANMAR", "MYANMAR (BURMA)") %>%
    str_replace_all("WEST BANK", "ISRAEL") %>%
    str_replace_all("DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO") %>%
    str_replace_all("IVORY COAST", "COTE D'IVOIRE") %>%
    str_replace_all("FEDERATED STATES OF MICRONESIA",
                    "MICRONESIA, FED. STATES OF") %>%
    str_replace_all("PUERTO RICO","USA TERRITORY")


  str_trim(country) #returns country name
}


#' fix_countries
#'
#' @description Checks that the country in which the location
#' (given by the columns LATITUDE and LONGITUDE) is the same with the
#' value from COUNTRY column. If there are differences, the value
#' from the COUNTRY column is updated.
#'
#' @details To determine the location's country,
#' we use the coords2country function. If the function coords2country
#' return NA, we don't update the value from the COUNTRY column.
#'
#' @param dff A dataframe containing NOAA Earthquake data
#' @return A date frame with the values from COUNTRY corrected
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' dfnew <- load_data()
#' dfnew <- dplyr::mutate(dfnew, LATITUDE=as.numeric(LATITUDE))
#' dfnew <- dplyr::mutate(dfnew, LONGITUDE=as.numeric(LONGITUDE))
#' dfnew <- dplyr::mutate(dfnew, EQ_PRIMARY=as.numeric(EQ_PRIMARY))
#' dfnew <- dplyr::mutate(dfnew, DEATHS=as.numeric(DEATHS))
#' dfnew <- fix_na_latitude(dfnew)
#' dfnew <- fix_na_longitude(dfnew)
#' dfnew <- fix_na_lon_na_lat(dfnew)
#' dfnew <- dplyr::mutate(dfnew, DataEQ=convert_data(YEAR, MONTH, DAY))
#' test <- fix_countries(dfnew)
#'
fix_countries <- function(dff)
{
  newCountry <- coords2country(dff$LATITUDE, dff$LONGITUDE)
  dff <- dff %>% mutate(newCountry=newCountry)

  cond <- !is.na(dff$newCountry) & dff$COUNTRY!=dff$newCountry

  condPortugal <- dff$COUNTRY=="AZORES (PORTUGAL)" &
    dff$newCountry=="PORTUGAL"
  id <- dff$I_D[condPortugal & cond]
  dff$newCountry[dff$I_D %in% id] <- "AZORES (PORTUGAL)"
  idc <-c(id)

  condSpain <- dff$COUNTRY=="CANARY ISLANDS" &
    dff$newCountry=="SPAIN"
  id <- dff$I_D[condSpain & cond]
  dff$newCountry[dff$I_D %in% id] <- "CANARY ISLANDS"
  idc <-c(idc, id)

  condUK <- dff$COUNTRY=="UK TERRITORY" &
    dff$newCountry=="SAINT HELENA"
  id <- dff$I_D[condUK & cond]
  dff$LOCATION_NAME[dff$I_D %in% id] <- "SAINT HELENA"
  dff$newCountry[dff$I_D %in% id] <- "UK TERRITORY"

  idc <-c(idc, id)

  cond <- !is.na(dff$newCountry) & dff$COUNTRY!=dff$newCountry
  cond3 <- dff$newCountry  %in% c("CHINA", "TANZANIA", "USA",
                                  "POLAND", "ROMANIA", "BOLIVIA",
                                  "EGYPT", "MACEDONIA", "MONGOLIA",
                                  "MYANMAR (BURMA)", "CANADA",
                                  "KAZAKHSTAN", "KYRGYZSTAN",
                                  "MOZAMBIQUE", "PANAMA","GEORGIA",
                                  "FRANCE", "IRELAND", "CHILE",
                                  "KOSOVO")
  dff$LOCATION_NAME[cond & cond3] <- NA


  id <- dff$I_D[cond]
  dff$COUNTRY[cond] <- dff$newCountry[cond]

  dff <- dff %>% select(-newCountry)
  dff

}



#' eq_clean_data
#'
#' @description  A new dateframe is created (a cleaned dataframe) from a raw dataframe
#' by uniting the year, month, day in a single Date column (named DataEQ)
#' and converting some columns (LATITUDE, LONGITUDE, EQ_PRIMARY and DEATHS)
#' to numeric.Also I realized data imputation
#' using `fix_na...` functions.
#'
#' @param df A dataframe containing NOAA Earthquake data
#' @return A cleaned date frame
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' df <- load_data()
#' df <- eq_clean_data(df)
#'
eq_clean_data <- function(df)
{
  LATITUDE <- NULL
  LONGITUDE <- NULL
  EQ_PRIMARY <- NULL
  DEATHS <- NULL
  YEAR <- NULL
  MONTH <- NULL
  DAY <- NULL

  df <- df %>%
    mutate(LATITUDE=as.numeric(LATITUDE)) %>%
    mutate(LONGITUDE=as.numeric(LONGITUDE)) %>%
    mutate(EQ_PRIMARY=as.numeric(EQ_PRIMARY)) %>%
    mutate(DEATHS=as.numeric(DEATHS)) %>%
    fix_na_latitude %>%
    fix_na_longitude %>%
    fix_na_lon_na_lat %>%
    mutate(DataEQ=convert_data(YEAR, MONTH, DAY)) %>%
    fix_countries

  df
}

#' geom_timeline
#'
#' @description A ggplot2 graphical function to plot a timeline of earthquakes from cleaned data.
#' The plot indicates the magnitude of each earthquake and number of deaths.
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot2 graphical object
#'
#' @importFrom ggplot2 layer
#' @export
#'
#' @examples
#'
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- eq_location_clean(df)
#
#' country <- c("JAPAN")
#' dfFilter <- filterData(df, COUNTRY=country, minYear=1000, maxYear=1400)
#'
#' test <- (ggplot2::ggplot()+
#'            geom_timeline(data=dfFilter,ggplot2::aes(x=DataEQ,
#'                                            xmin=as.Date("1000-01-01"),
#'                                            xmax=as.Date("1400-12-31"),
#'                                            colour=DEATHS,
#'                                            fill=DEATHS,
#'                                            size=EQ_PRIMARY,
#'                                            y=COUNTRY)))
#'
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geomTimeline, mapping = mapping,
    data = data,  stat=stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' geomTimeline
#' @description Function to construct a new class for the geom_timeline
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} date
#'   \item \code{y} COUNTRY
#'   \item \code{xmin} minimum date for earthquakes
#'   \item \code{xmax} maximum date for earthquakes
#'   \item \code{size} size of shape has the value of EQ_PRIMARY
#'   \item \code{fill} fill colour shape (number of deaths) eg DEATHS
#'   \item \code{colour} colour shape (number of deaths) eg DEATHS
##' }
#'
#' @importFrom ggplot2 ggproto
#' @importFrom grid segmentsGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom grid xaxisGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
#'
geomTimeline <- ggplot2::ggproto("geomTimeline", ggplot2::Geom,
                                 required_aes = c("x",
                                                  "xmin",
                                                  "xmax"
                                 ),
                                 optional_aes = c("y"),
                                 default_aes = ggplot2::aes(shape = 19,
                                                            size = 1,
                                                            colour = "grey",
                                                            fill = "grey",
                                                            alpha = 0.5,
                                                            stroke = 1,
                                                            y = 0.5),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_group = function(data, panel_scales, coord) {


                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)



                                   # draw a line (timeline) for earthquake points (with segmentGrob)
                                   # the earthqauke points will be draw on this timeline
                                   x0 <- coords$xmin
                                   y0 <- coords$y
                                   x1 <- coords$xmax
                                   y1 <- coords$y
                                   paramSeg <- grid::gpar(col = "grey", alpha = 0.15)
                                   segment <- grid::segmentsGrob(x0 = x0, y0=y0, x1= x1, y1 = y1,gp = paramSeg)

                                   # we will set the parameters (shape, size, colour, fill, alpha)
                                   # for those points (positions in time of earthquakes)
                                   #
                                   xPoint <- grid::unit(coords$x,"native")
                                   yPoint <- coords$y
                                   shapePoint <- coords$shape
                                   sizePoint <- grid::unit(coords$size,"mm")
                                   paramPoint <-grid::gpar(col = coords$colour, fill = coords$fill,
                                                           alpha = coords$alpha)

                                   # draw earthquakes suing pointsGrob
                                   point <- grid::pointsGrob(x = xPoint, y=yPoint, pch=shapePoint,
                                                             size = sizePoint, gp = paramPoint)

                                   # draw x axis and set the parameters for its
                                   paramXaxis <- grid::gpar(col = "grey21", alpha = 0.95)
                                   Xaxis <- grid::xaxisGrob(gp = paramXaxis)


                                   ObjList<- grid::gList(segment, Xaxis, point)
                                   grid::gTree(children = ObjList)

                                 })


#' setupGeom
#'
#' @description Create a ggplot theme and add this
#' to an ggplot object.
#'
#' @param p a ggplot object previously built
#' @return a ggplot object
#'
#' @export
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_fill_continuous
#' @importFrom ggplot2 scale_colour_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#'
#' @examples
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- eq_location_clean(df)
#' country <- c("JAPAN")
#' dfFilter <- filterData(df, COUNTRY=country,
#' minYear=1000, maxYear=1400)
#' test <- (ggplot2::ggplot()+
#'            geom_timeline(data=dfFilter,ggplot2::aes(x=DataEQ,
#'                                            xmin=as.Date("1000-01-01"),
#'                                            xmax=as.Date("1400-12-31"),
#'                                            colour=DEATHS,
#'                                            fill=DEATHS,
#'                                            size=EQ_PRIMARY,
#'                                            y=COUNTRY))+
#'            geom_timeline_label(data=dfFilter, ggplot2::aes(x=DataEQ,
#'                                                   location=LOCATION_NAME,
#'                                                   size=EQ_PRIMARY,
#'                                                   n_max=5,
#'                                                   y=COUNTRY)))
#'
#' a <- setupGeom(test)
#'
setupGeom <- function(p)
{
  theme_timeline <-  ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold"),
                   axis.line.y =ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   legend.box = "horizontal",
                   legend.direction = "horizontal",
                   legend.position = "bottom")
  p<-p+
    ggplot2::scale_size_continuous(name = "Richter scale values")+
    ggplot2::scale_fill_continuous(name = "# Deaths")+
    ggplot2::scale_colour_continuous(name = "# Deaths")
  ggplot2::scale_alpha_continuous(name = "# Deaths")


  p<- p+ggplot2::xlab("DATE")+theme_timeline
  p
}

#' filterData
#'
#' @description A cleaned NOOA dataframe is prepared for
#' visualisation. That means that the eartquakes
#' are filtered after a vector of \code{COUNTRY}  and
#' a period of time. The period of time is given between
#' a \code{minYear} and \code{maxYear}.
#'
#' @param df a NOOA Dataframe
#' @param COUNTRY a vector of characters representing
#' country after which we will filter
#' @param minYear a minimum value for YEAR
#' @param maxYear a maximum value for YEAR
#' @return a filtered dataframe
#'
#' @export
#' @importFrom dplyr between
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang is_na
#'
#' @examples
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- eq_location_clean(df)
#' dff <- filterData(df, COUNTRY="CUBA", minYear=1880, maxYear=1992)
#'
filterData <- function(df, COUNTRY, minYear=2000, maxYear=2001)
{
  minYear <- ifelse(rlang::is_na(minYear[1]),2000, minYear[1])
  maxYear <- ifelse(rlang::is_na(maxYear[1]),2001, maxYear[1])

  cond <- dplyr::between(df$YEAR, minYear, maxYear)
  if(!missing(COUNTRY))
  {
    cond <- cond & (df$COUNTRY %in% COUNTRY)
  }

  YEAR <- NULL
  LOCATION_NAME <- NULL
  EQ_PRIMARY <- NULL
  DEATHS <- NULL
  DataEQ <- NULL

  df <- dplyr::select(df, YEAR, COUNTRY, LOCATION_NAME, EQ_PRIMARY, DEATHS, DataEQ)
  df <- dplyr::filter(df, cond)
  df$LOCATION_NAME[is.na(df$LOCATION_NAME)] <- "Unknown location"
  df$EQ_PRIMARY[is.na(df$EQ_PRIMARY)] <- 1
  df

}

#' geom_timeline_label
#'
#' @description This geom adds a vertical line to
#' each data point with a text annotation
#' (e.g. the location of the earthquake)
#' attached to each line. and number of deaths.
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot2 graphical object
#'
#' @importFrom ggplot2 layer
#' @export
#'
#' @examples
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- eq_location_clean(df)
#'
#' country <- c("JAPAN")
#' dfFilter <- filterData(df, COUNTRY=country, minYear=1000, maxYear=1400)
#'
#' test <- (ggplot2::ggplot()+
#'            geom_timeline(data=dfFilter,ggplot2::aes(x=DataEQ,
#'                                            xmin=as.Date("1000-01-01"),
#'                                            xmax=as.Date("1400-12-31"),
#'                                            colour=DEATHS,
#'                                            fill=DEATHS,
#'                                            size=EQ_PRIMARY,
#'                                           y=COUNTRY))+
#'            geom_timeline_label(data=dfFilter, ggplot2::aes(x=DataEQ,
#'                                                   location=LOCATION_NAME,
#'                                                   size=EQ_PRIMARY,
#'                                                   n_max=5,
#'                                                   y=COUNTRY)))
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geomTimelineLabel, mapping = mapping,
    data = data,  stat=stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' geomTimelineLabel
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} date
#'   \item \code{location} name of the location of earthquake
#'   \item \code{size} size of shape has the value of EQ_PRIMARY
#'   \item \code{y} country where was the earthquake
#'   \item \code{n_max} maximux number of lation which is ploted
##' }
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 zeroGrob
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr slice
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
#'
geomTimelineLabel <- ggplot2::ggproto("geomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "location", "size"),
                             optional_aes = c("y","n_max"),
                             default_aes = ggplot2::aes(n_max=0, y = 0.5),
                             draw_key = ggplot2::draw_key_blank,
                             draw_panel = function(data, panel_scales, coord) {

                               n_max <- data$n_max[1]
                               if(n_max<=0)
                               {
                                 #don't draw nothing
                                 return(ggplot2::zeroGrob())
                               }

                               # create a list with dataframes
                               # 1. sort dataframe after y and size
                               # 2. groups dataframe after y and the put
                               # each group in an element of a list
                               # 3. from each dataframe keep just
                               # first n_max rows
                               list_data<-data %>%
                                 dplyr::arrange(y,-size) %>%
                                 split(.$y) %>%
                                 purrr::map(dplyr::slice,1:n_max[1])

                               #create a new dataframe uniting the dataframes from list
                               data <- dplyr::bind_rows(list_data)


                               ## Transform the data first
                               coords <- coord$transform(data, panel_scales)


                               #create a vertical line
                               #at the en of this line we write the name of location
                               #of earthquake
                               step <- 0.03
                               vertical_line <- grid::segmentsGrob(x0=coords$x,
                                                                   y0=coords$y,
                                                                   x1=coords$x,
                                                                   y1=coords$y+step,
                                                                   gp=grid::gpar(col = "grey", alpha = 0.85))

                               # create a text (rotated with 45 degree)
                               # reprezenting the name of location
                               label_loc <-grid::textGrob(label=coords$location,
                                                          x=coords$x,
                                                          y=coords$y+step+0.02,
                                                          rot=45,
                                                          just="left",
                                                          gp = grid::gpar(fontsize = 9))

                               # put the grobs objects in a list and create a
                               # newgrid graphical bject which is sent to output
                               grid::gTree(children = grid::gList(label_loc, vertical_line))

                             })

#' create_map
#'
#' @description The function draw the epicenters eartquakes
#' (LATITUDE/LONGITUDE) from a filtered NOAA dataframe.
#' Each point is annotates with an pop up window containing
#' annotation data stored in a column of the data frame.
#'
#' @details Using the columns from filtered frameframe,
#' we create (for each location on the map) an HtML string
#' (which is displayed in the pop-up window).
#  Those HTMl strings are stored in the vector \code{content}.
#'
#' @param df A NOAA dataframe
#' @param content a string vector which contains HTML code
#' @return A leaflet object (a map)
#'
#' @export
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr %>%
#'
#' @examples
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- eq_location_clean(df)
#'
#' dff <- dplyr::filter(df, COUNTRY=="CHINA" & YEAR>=2010)
#' magnitude <- ifelse(is.na(dff$EQ_PRIMARY),4,dff$EQ_PRIMARY)
#'
#' content <- paste("<b>Location:</b>", df$LOCATION_NAME,"<br/>",
#'                  "<b>Magnitude:</b>", df$EQ_PRIMARY,"<br/>",
#'                  "<b>Total deaths:</b>", df$TOTAL_DEATHS)
#'
#' a <- create_map(dff, content)
#'
create_map <- function(df, content)
{
  magnitude <- ifelse(is.na(df$EQ_PRIMARY),4,df$EQ_PRIMARY)
  mymap <- leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data=df,lat= ~LATITUDE,
                              lng = ~LONGITUDE,
                              popup = content,
                              radius=~magnitude,
                              color='blue', weight=2,
                              stroke =TRUE, fillOpacity = 0.2)

  mymap
}


#' eq_map
#'
#' @description The function show on a map eartquakes for a
#' particular country
#'
#' @param df A NOAA dataframe filterd after COUNTRY and YEAR
#' @param annot_col A string representing the name of column
#' used to annotate earthquake marker
#' @return A a map displaying earthquate location for a given country with user defined popup
#'
#' @export
#' @importFrom stringr str_replace
#'
#' @examples
#'
#' df <- load_data()
#' df <- eq_clean_data(df)
#' df <- dplyr::filter(df, COUNTRY == "MEXICO" & lubridate::year(DataEQ) >= 2000)
#' g <- eq_map(df, annot_col = "DataEQ")
#'
eq_map <- function(df, annot_col="popup text")
{
  coll <- annot_col[1]

  df$EQ_PRIMARY <- as.numeric(df$EQ_PRIMARY)
  #magnitude <- ifelse(is.na(df$EQ_PRIMARY),4,df$EQ_PRIMARY)

  if(coll[1]=="popup text")
  {
    content <- paste("<b>Location:</b>",
                           df$LOCATION_NAME,
                           "<br/>",
                           "<b>Magnitude:</b>",
                           df$EQ_PRIMARY,
                           "<br/>",
                           "<b>Total deaths:</b>",
                           df$TOTAL_DEATHS) %>%
      stringr::str_replace("<b>Location:</b> NA","") %>%
      stringr::str_replace("<br/> <b>Total deaths:</b> NA","") %>%
      stringr::str_replace("<br/> <b>Magnitude:</b> NA","")

    mymap <- create_map(df, content)
    return(mymap)
  }

  if(coll[1] %in% names(df))
  {
    df <- df %>% dplyr::filter(!is.na(coll[1]))
    #getElement(df, "YEAR") is equivalent with df$YEAR
    #content <- paste("<b>",coll[1],":</b>", getElement(df,coll[1]))
    content <- paste(getElement(df,coll[1]))
  } else {
    stop(paste(coll[1]," is not column of your dataframe!"))
  }

  mymap <- create_map(df, content)

  return(mymap)
}
