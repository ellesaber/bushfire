## PROCESSING FIRE OCCURENCE DATA ##
#relevant Packages
library(sp)
library(rgdal)
library(raster)
library(data.table)
library(ggmap)
library(dplyr)

# read in CSV of fire occurence data
fires.raw <- read.csv("./data_raw/grassfire.csv", head=TRUE, as.is=TRUE)
# Sense check on import
summary(fires.raw)

# easting should not be more than 6 figures and northing not more than 5000000**
# convert incident_date&time to date format add in column with date only
x <- as.Date(fires.raw$incident_date, "%d/%m/%y %H:%M:%S")
fires.raw$date <- x
# remove comma and turn eastings and northings into numbers
fires.raw$amg_e <- as.numeric(gsub(",", "", fires.raw$amg_e))
fires.raw$amg_n <- as.numeric(gsub(",", "", fires.raw$amg_n))

# Check conversion worked
head(fires.raw)


## Convert AMG coordinates to Longitude and Latitude ##
# prepare UTM coordinates matrix
# subset for different zones
zone55 <- filter(fires.raw, amg_zone==55)
zone54 <- filter(fires.raw, amg_zone==54)
# fires.other <- subset(fires, amg_zone != 55 & amg_zone != 54)
# convert to spatial points with amg projection
utmcoor55 <- SpatialPoints(cbind(zone55$amg_e, zone55$amg_n), proj4string=CRS("+proj=utm +zone=55 +south +ellps=aust_SA +units=m +no_defs")) 
utmcoor54 <- SpatialPoints(cbind(zone54$amg_e, zone54$amg_n), proj4string=CRS("+proj=utm +zone=54 +south +ellps=aust_SA +units=m +no_defs ")) 
# zone55$amg_e and zone55$amg_n are corresponding to AMG Easting and Northing, respectively.

# converting from australian map grid to longitude and lattitude 
longlatcoor55 <- spTransform(utmcoor55,CRS("+proj=longlat"))
longlatcoor54 <- spTransform(utmcoor54,CRS("+proj=longlat"))
## Convert Spatial dataframes to normal data frames
longlat55 <- as.data.frame(longlatcoor55)
longlat54 <- as.data.frame(longlatcoor54)
# Add in Longlat values to fire
fires.55 <- data.frame(date = zone55$date, suburb = zone55$suburb, 
                       size = zone55$size, 
                       long = longlat55$coords.x1, 
                       lat = longlat55$coords.x2)
fires.54 <- data.frame(date = zone54$date, suburb = zone54$suburb, 
                       size = zone54$size, 
                       long = longlat54$coords.x1, 
                       lat = longlat54$coords.x2)
fires.df <- rbind(fires.55, fires.54)

# remove non-conformative long lats i.e. outside +-90, +-180
fires.df <- filter(fires.df, between(lat, -90, 90))
fires.df <- filter(fires.df, between(long, -180, 180))

# load in shp file
# load in shape file
# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "STE11aAust"  
# Read in the data
aus.shape <- readOGR(dsn="./data_raw/shps", layer=layerName) 
summary(aus.shape)


## subset for dates before 1/1/2010 to align with weather data time span
enddate <- as.Date("1/1/10", "%d/%m/%y") 
fires.df <- filter(fires.df, date < enddate )
# create spatial points from longlats
firepoints <- SpatialPoints(cbind(fires.df$long, fires.df$lat))
# code from stack overflow on how to get states from long lat coordinates
proj4string(firepoints) <- proj4string(aus.shape)
# as characters
result <- as.character(over(firepoints, aus.shape)$STATE_NAME)
# sense check
summary(result)
fires.df$state <- result
fires.df <- filter(fires.df, state == "Victoria")

save(fires.df, file="./robject/fires.rda")

#### Find closest point in weather data ####
# Load file containing w_e&s_n coordinates corresponding to Victoria
load("./robject/viclonlat.rda")

# convert long lats to spatial points clas
vicpoints <- SpatialPoints(cbind(vic.dt$LONG, vic.dt$LAT), proj4string =CRS("+proj=longlat"))
firepoints <- SpatialPoints(cbind(fires.df$long, fires.df$lat), proj4string =CRS("+proj=longlat"))

#  Define the vectors, used in the loop
closestSiteVec <- vector(mode = "numeric",length = nrow(fires.df))
minDistVec     <- vector(mode = "numeric",length = nrow(fires.df))

# Loop to find min distance point
for (i in 1 : nrow(fires.df))
{
  # gives distance between fire and vict gridpoints
  distVec <- spDistsN1(vicpoints,firepoints[i,],longlat = TRUE)
  # finds min distance between fire and vic gridpoints (for sense check)
  minDistVec[i] <- min(distVec)
  # gives row references for the vic point closest to the fire
  closestSiteVec[i] <- which.min(distVec)
}

# extract s_n and w_e points assoctiated with min distance
assign.s_n <- as(vic.dt[closestSiteVec, ]$s_n, "numeric")
assign.w_e <- as(vic.dt[closestSiteVec, ]$w_e,"numeric")
#Produce final data table
fire.dt <- data.table(date=fires.df$date, suburb=fires.df$suburb,size=fires.df$size,
                         long=fires.df$long,lat=fires.df$lat,s_n=assign.s_n,w_e=assign.w_e)


# Save fires file
save(fire.dt, file="./robject/firedt.rda")

#### Add Fire Dummy ####

# load files
load("./robject/dtfull.rda")

# convert dates from origin of 31/12/1999 and make new column
dt.full$date <- as.Date(dt.full$date, origin="1999-12-31")

# make new column for fire occurence dummy filled with zeros
# duplicate values in fire occurence data, currently deleting duplicates*
# need to check if it is a mistake or not and amalgamate them if not*
fire.dt[ ,fire:=1L]
fire.dt0 <- fire.dt[!duplicated(fire.dt, by=c("date", "s_n", "w_e"))]
dat <- left_join(dt.full, fire.dt0, by = c("date", "s_n", "w_e"))
dat <- dat[is.na(fire), fire := 0]

dat <- dplyr::select(dat, date, s_n, w_e, temp, LAT, LONG, district, div, subdiv, ffdi, df, rh, ws, fire)

oldnames <- colnames(dat)
newnames <- c("date", "s_n", "w_e", "temp", "lat", "long", "district","div", "subdiv", "ffdi", "df", "rh", "ws", "fire")
setnames(dat, oldnames, newnames)

#### Subsetting Data set ####

# subset to only coordinates where fires have occurred
fire.dt$coord <- paste(fire.dt$s_n, fire.dt$w_e, sep=".")
dat$coord <- paste(dat$s_n, dat$w_e, sep=".")

dat0 <- dplyr::filter(dat, coord %in% fire.dt$coord)

# subset to fire season only
# create month variable
dat0[ ,month:=month(dat0$date)]

# fire season consists of oct, nov, dec, jan, feb, march
fireseas <- c(10, 11, 12, 1, 2, 3)
# filter dataset to only include fire season
dat0 <- dplyr::filter(dat0, month %in% fireseas)

dat0$div <- as.factor(dat0$div)
dat0$subdiv <- as.factor(dat0$subdiv)

save(dat0, file="./robject/dat0.rda")

## REFERENCES ##
#Google site code on how to convert (https://sites.google.com/a/lakeheadu.ca/yong-luo/blog/convert-utm-to-longlat-in-r)
# Using specs for amg from http://spatialreference.org/ref/?search=amg 
# Used example to find closest points https://www.nceas.ucsb.edu/scicomp/usecases/AssignClosestPointsToPoints

