library(rvest)
library(xml2)
library(data.table)
library(dplyr)
library(raster)

#### Download vapour files ####
# list websites that contain the right 
webs <- list ("http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2000/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2001/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2002/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2003/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2004/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2005/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2006/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2007/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2008/catalog.html",
              "http://opendap.bom.gov.au:8080/thredds/catalog/daily_vapour_pressure_9am_5km/2009/catalog.html")

# preamble required before address for downloading files
pre <- "http://opendap.bom.gov.au:8080/thredds/fileServer/daily_vapour_pressure_9am_5km/"

# function that reads the xml nodes for each website
get_html_text <- function(url, css_or_xpath="*"){
  html_text(
    html_nodes(
      read_html(url), css_or_xpath
    )
  )
}
# gets list of file urls to download 
ls <- lapply(webs, get_html_text, css_or_xpath="tr~ tr+ tr a tt")
ls <- unlist(ls)
# filters to only include fire season
sub <- which(substring(ls, 52, 53) %in% c("01", "02", "03", "10", "11", "12"))
ls <- ls[sub]

# makes file urls 
fileurl <- paste0(pre, substring(ls, 48, 51), "/", ls)
filename <- paste0("./data_raw/vapour/", substring(ls, 48, 58))

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

dates <- substring(filename, 19,26)
# renames columns 
oldnames <- colnames(ext)
setnames(ext, oldnames, dates)

# bind with coords
vap.dt <- as.data.table(cbind("coord"=paste(vic.dt$s_n, vic.dt$w_e, sep="."), ext))
# put into long format so it can be joined to full dataset
vap.dt <- as.data.table(gather(vap.dt, key="date", value="vapour", col=-1))
vap.dt[,"date":= as.Date(date, "%Y%m%d")]

# save vap.dt
save(vap.dt, file="./robject/vapour.rda")

#### add into dataset ####

# load in datatable
load("./robject/dat0.rda")

# add vapour to datatable
dat0 <- left_join(dat0, vap.dt, by=c("coord", "date"))
# save!
save(dat0, file="./robject/dat0.rda")

