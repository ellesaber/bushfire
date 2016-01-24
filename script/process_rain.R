library(rvest)
library(xml2)
library(data.table)
library(dplyr)
library(raster)
library(tidyr)

#### Download rain files ####
# list of urls with file urls 
webs <- list ("http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2000/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2001/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2002/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2003/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2004/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2005/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2006/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2007/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2008/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_rain_5km/2009/catalog.html"
              )
# preamble required for file url
pre <- "http://opendap.bom.gov.au:8080/thredds/fileServer/daily_rain_5km/"

# function that gets file urls from websites 
get_html_text <- function(url, css_or_xpath="*"){
    html_text(
      html_nodes(
        read_html(url), css_or_xpath
              )
            )
          }
# lists file urls 
ls <- lapply(webs, get_html_text, css_or_xpath="tr~ tr+ tr a tt")
ls <- unlist(ls)

# subsets to only include fire season
sub <- which(substring(ls, 41, 42) %in% c("01", "02", "03", "10", "11", "12"))
ls <- ls[sub]

fileurl <- paste0(pre, substring(ls, 37, 40), "/", ls)
filename <- paste0("./data_raw/rain/", substring(ls, 37, 47))

# download files 
for (i in 1:length(fileurl)) 
{
  download.file(fileurl[i], filename[i], 
                method = "auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
}

#### Read in files ####

# read in WRF gridpoints 
load("./robject/viclonlat.rda")

# number of files
nfiles <- length(filename)
# create datatable to fill with extract loop
rain.dt <- data.table("coord"=paste(vic.dt$s_n, vic.dt$w_e, sep="."))
lonlat <- dplyr::select(vic.dt, LONG, LAT)

# for each raster of grassland curing in the relevant time periods:
## read in file

## file the median curing value in a 5k buffer around each lon/lat coord in vic
## add these values to a datatable. 
# open all the files as rasters
ras <- lapply(filename,raster) 
# turns into RasterStack
STACK <- stack(ras)
e <- extent(140, 150, -40, -30) #upper and lower limits to contain VIC
# crops as per above limits
rc <- crop(STACK, e)
# extracts median value from 5000m radius around lonlat points
ext <- as.data.table(raster::extract(rc, lonlat, buffer=5000, fun=median))

dates <- substring(filename, 17,24)

# rename columns of datatable 
oldnames <- colnames(ext)
setnames(ext, oldnames, dates)

# combines precip values with coordinates
rain.dt <- as.data.table(cbind("coord"=paste(vic.dt$s_n, vic.dt$w_e, sep="."), ext))

# put into long form so can be merged with main dataset 
rain.dt <- as.data.table(gather(rain.dt, key="date", value="rain", col=-1))
rain.dt[,"date":= as.Date(date, "%Y%m%d")]

# save rainfall
save(rain.dt, file="./robject/rain.rda")
#### add into dataset ####

# load in datatable
load("./robject/dat0.rda")

# add rain to datatable
dat0 <- left_join(dat0, rain.dt, by=c("coord", "date"))

# save!
save(dat0, file="./robject/dat0.rda")

