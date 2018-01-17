

# NOOA package

This package was designed to provide visualising methodes for earthquake data. These data were provided by the [U.S. National Oceanographic and Atmospheric Administration (NOAA)](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) Significant Earthquake Database. 
The data set contains information on 5,933 earthquakes that occurred in a 4,000-year period. The data set is in the directory `date` from the package (see the file `signif.txt`). 

##Installation

The NOOA package can be installed from github using the commands:
```R
library(devtools)
install_github("constantin345/NOOA")
library(NOOA)
```

## Description

The package provides the user the next functions:

* data loading functions:
    + `load_data`
    
* data imputation functions:
    + `fix_na_latitude`
    + `fix_na_longitude`
    + `fix_na_lon_na_lat`
    
* data cleaning functions:
    + `eq_clean_data`
    + `eq_location_clean`
    + `fix_countries`
    + `remove_countries`
    + `convert_data`
    
* data visualisation functions:
    + `geomTimeline`
    + `geom_timeline`
    + `geomTimelineLabel`
    + `geom_timeline_label`
    + `eq_map`
    
* help functions:
    + `setupGeom`
    + `filterData`
    + `create_map`
    + `coords2country`

    
Information about how to use these functions is found in `vignettes` directory.



##Examples
How to use geom_timeline_label and geom_timeline:
```{r}
library(dplyr)
library(ggplot2)
library(NOOA)

#load data and clean data
df <- load_data() %>% eq_clean_data %>% eq_location_clean

#create a filtered dataframe
country <- c("JAPAN")
dfFilter <- filterData(df, COUNTRY=country, minYear=1000, maxYear=1400)

#create graphic
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

g <- setupGeom(test)
g
```

![](man/figures/README-example-1.png)    
    
    
How to use eq_map:

``` r
library(dplyr)
library(ggplot2)
library(leaflet)
library(NOOA)
dff <- load_data() #load data
dff <- eq_clean_data(dff) #clean data
dff <- filter(dff, COUNTRY == "MEXICO" & lubridate::year(DataEQ) >= 2000) #create a filtered dataframe

g <- eq_map(dff, annot_col = "DataEQ") 
g
```
![](man/figures/README-example-2.png)
