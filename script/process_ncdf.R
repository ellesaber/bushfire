#### Packages ####

library(ncdf)
library(raster)
library(rgdal)
library(sp)
library(data.table)
library(dplyr)

#### Get lon/lats from smaller NCDF file ####

# Read netcdf into R
subset.name <- "./data_raw/weather/tmax_subset.nc"
sub.temp <- open.ncdf(subset.name)

# Extract the geo info
# Grab the lat and lon from the data
latmat <- raster(subset.name, varname = "XLAT")
lonmat <- raster(subset.name, varname = "XLONG")
# for each WE-SN coordinate get Long and Lat 

# Convert rasters to dataframe
plat <- data.frame(rasterToPoints(latmat))
colnames(plat) <- c("w_e", "s_n", "LAT")
plon <- data.frame(rasterToPoints(lonmat))
colnames(plon) <- c("w_e", "s_n", "LONG")
df.geo <- left_join(plat, plon, by=c("w_e", "s_n"))

# Create data frame with just longitude and latitude and switch order
lonlat <- dplyr::select(df.geo, LONG, LAT)
# create spatial points from long lat coordinates
points <- SpatialPoints(lonlat)

#### Use shp file to find Aus states from Long and Lat ####

# Download the shp file of Aus states and territories from ABS
url <- 'http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1259030001_ste11aaust_shape.zip&1259.0.30.001&Data%20Cubes&D39E28B23F39F498CA2578CC00120E25&0&July%202011&14.07.2011&Latest'
file <- paste("./data_raw/shps",basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir="./data_raw/shps")
}

# Show the unzipped files 
list.files("./data_raw/shps")

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "STE11aAust"  
# Read in the data
aus.shape <- readOGR(dsn="./data_raw/shps", layer=layerName) 
# remove other polygons to leave only Victoria
vic.shp <- aus.shape[aus.shape$STATE_NAME == "Victoria",]

# code from stack overflow on how to get states from long lat coordinates
proj4string(points) <- proj4string(vic.shp)
# as characters
result <- as.character(over(points, vic.shp)$STATE_NAME)
# sense check
summary(result)
df.geo$state <- result

#### Add in statistical division ####

# Download shp file of statistical divisions from ABS 
url <- 'http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1259030001_sd11aaust_shape.zip&1259.0.30.001&Data%20Cubes&A2521D72ABA3D177CA2578CC0011FBFE&0&July%202011&14.07.2011&Latest'
file <- paste("./data_raw/shps",basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir="./data_raw/shps")
}

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "SD11aAust"  
# Read in the data
aus.shape <- readOGR(dsn="./data_raw/shps", layer=layerName) 
#remove other polygons to leave only Victoria
vic.shp <- aus.shape[aus.shape$STATE_CODE == 2,]

# code from stack overflow on how to get states from long lat coordinates
proj4string(points) <- proj4string(vic.shp)
# as characters
result <- as.character(over(points, vic.shp)$SD_NAME11)
# sense check
summary(result)
df.geo$div <- result

#### Add in statistical subdivision ####

# Download shp file of statistical subdivisions from ABS 
url <- 'http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1259030001_ssd11aaust_shape.zip&1259.0.30.001&Data%20Cubes&4913953D966B9E79CA2578CC0011E833&0&July%202011&14.07.2011&Latest'
file <- paste("./data_raw/shps",basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir="./data_raw/shps")
}

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "SSD11aAust"  
# Read in the data
aus.shape <- readOGR(dsn="./data_raw/shps", layer=layerName) 
# remove other polygons to leave only Victoria
vic.shp <- aus.shape[aus.shape$STATE_CODE == 2,]

# code from stack overflow on how to get states from long lat coordinates
proj4string(points) <- proj4string(vic.shp)
# as characters
result <- as.character(over(points, vic.shp)$SSD_NAME11)
# sense check
summary(result)
df.geo$subdiv <- result
vic.dt <- data.table(filter(df.geo, state == "Victoria"))

#### Add in district vic points ####
layerName <- "cfa_district" 
cfa.dist <- readOGR(dsn="./data_raw/shps", layer=layerName) 

# check out shape file
summary(cfa.dist)
viclonlat <- dplyr::select(vic.dt, LONG, LAT)
viclonlat <- SpatialPoints(viclonlat)
proj4string(viclonlat) <- proj4string(cfa.dist)
# find the state for each coordinaate using the shp file (*takes awhile)
result <- as.factor(over(viclonlat, cfa.dist)$CFA_DIST)
# sense check
head(result)
summary(result)

# add district column to vic.dt
vic.dt[ , "district":=result]

# Save file
save(vic.dt, file="./robject/viclonlat.rda")

#### Process all NCDF files, subset for Victoria and 2000-2010 #### 

# Read netcdf files into R
tmaxfile <- "./data_raw/weather/TMAX1984-2009.nc"
ffdifile <- "./data_raw/weather/FFDI1984-2009.nc"
dffile <- "./data_raw/weather/DF1984-2009.nc"
rhfile <- "./data_raw/weather/RH2_3pm_1984-2009.nc"
wsfile <- "./data_raw/weather/W1984-2009.nc"

tmax <- open.ncdf(tmaxfile)
ffdi <- open.ncdf(ffdifile)
df <- open.ncdf(dffile)
rh <- open.ncdf(rhfile)
ws <- open.ncdf(wsfile)

temp.arr <- get.var.ncdf(tmax, "TMAX")
tmax.time <- get.var.ncdf(tmax, "time")
ffdi.arr <- get.var.ncdf(ffdi, "FFDI")
df.arr <- get.var.ncdf(df, "DF")
rh.arr <- get.var.ncdf(rh, "RH2")
ws.arr <- get.var.ncdf(ws, "W10")

# changing time to dates
# Start date from Hamish 1/11/1984 
# dimensions of temp.arr = west_east X south_north X time
wrfstart <- as.Date("19841101", format="%Y%m%d") 
firestart <- as.Date("20000101", format="%Y%m%d") 
timestart <- as.numeric(firestart - wrfstart)
timeend <- nrow(tmax.time)-1

# subset to timesets only from 1/1/2000 till last value (November 2009)
temp.arr <- temp.arr[, ,timestart:timeend] 
ffdi.arr <- ffdi.arr[timestart:timeend, ,]
df.arr <- df.arr[timestart:timeend, , ]
rh.arr <- rh.arr[ , ,timestart:timeend]
ws.arr <- ws.arr[ , ,timestart:timeend]

# Convert arrays to data.table using melt, origin becomes 31/12/1999
# Melt function from data.table package not reshape 
temp.dt <- data.table(melt(temp.arr, varnames = c("w_e", "s_n", "date"),
                           value.name = "temp")) 
# remove array now we have a datatable (takes up too much memory!)
rm(temp.arr) 

ffdi.dt <- data.table(melt(ffdi.arr, varnames = c("date", "s_n", "w_e"),
                           value.name = "ffdi")) 
rm(ffdi.arr)

df.dt <- data.table(melt(df.arr, varnames = c("date", "s_n", "w_e"),
                         value.name = "df")) 
rm(df.arr)

rh.dt <- data.table(melt(rh.arr, varnames = c("w_e", "s_n", "date"),
                         value.name = "rh" ))

rm(rh.arr)

ws.dt <- data.table(melt(ws.arr, varnames = c("w_e", "s_n", "date"),
                         value.name = "ws" ))

rm(ws.arr)

# merge data tables #Is there a function to merge multiple data tables?#
dt.full <- inner_join(temp.dt, vic.dt, by = c("w_e", "s_n"))

dt.full <- left_join(dt.full, ffdi.dt, by = c("w_e", "s_n", "date"))
dt.full <- left_join(dt.full, df.dt, by = c("w_e", "s_n", "date"))
dt.full <- left_join(dt.full, rh.dt, by = c("w_e", "s_n", "date"))
dt.full <- left_join(dt.full, ws.dt, by = c("w_e", "s_n", "date"))

# Sense Check
head(dt.full)
summary(dt.full)

# Save dt.full for later use
save(dt.full, file="./robject/dtfull.rda")
