library(rvest)
library(xml2)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(raster)

#### Download curing files ####
web <- read_html("http://opendap.bom.gov.au:8080/thredds/catalog/curing_modis_500m_8-day/aust_regions/vic/netcdf/mapvictoria/catalog.html")

# the data we want is in the first table on this page
# the html_table() command coerces the data into a data frame
ls <- web %>%
  html_nodes("tr~ tr+ tr a tt") %>%
    html_text()

pre <- "http://opendap.bom.gov.au:8080//thredds/fileServer/curing_modis_500m_8-day/aust_regions/vic/netcdf/mapvictoria/"

#list of file URLS
fileurl <- paste(pre, ls, sep="")
#dates to add to files names
filename <- substring(fileurl,151,170)


#subset to years before 2010 and fire season only
sub <- which(substring(filename, 1, 4)<2010)
fs <- which(as.numeric(substring(filename, 5,6)) %in% c(1, 2, 3, 12, 11, 10) |
            as.numeric(substring(filename, 14,15)) %in% c(1, 2, 3, 12, 11, 10))
sub <- intersect(sub,fs)

# subset lists to only relevant years and months 
fileurl <- fileurl[sub]
filename <- filename[sub]
filename <- paste("./data_raw/curing/", filename, sep="")

#download file 
for (i in 1:length(fileurl)) 
{
  download.file(fileurl[i], filename[i], 
                method = "auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
}

#### Read in curing and match to wrf grids ####

#read in WRF gridpoints 
load("./robject/viclonlat.rda")

# number of files
nfiles <- length(filename)
# create datatable to fill with extract loop
curing.dt <- data.table("coord"=paste(vic.dt$s_n, vic.dt$w_e, sep="."))
lonlat <- dplyr::select(vic.dt, LONG, LAT)
  
# for each raster of grassland curing in the relevant time periods:
## read in file
## file the median curing value in a 5k buffer around each lon/lat coord in vic
## add these values to a datatable. 
for (j in 1:nfiles){
  #read in file
  ras <- raster(filename[j], varname="curing")
  #extract median values for buffer around points
  ex <- raster::extract(ras, lonlat, buffer=5000, fun=median)
  curing.dt[ ,substring(filename[j], 19,35):=as.numeric(ex)]
}


#### Make into nice datatable ####

#find start and end dates for each set of files
#start date for curing raster
strt <- as.Date(substring(filename, 19, 26),"%Y%m%d")
#date of the next file last entry is NA because no more files
nxt <- as.Date(substring(filename[-1], 19, 26),"%Y%m%d")-1
nxt[221] <- as.Date("20091231", "%Y%m%d")
#fill end date with the earliest date of the file name
# or the day before the next file begins

end <- mapply(min, as.Date(substring(filename, 28, 35),"%Y%m%d"), nxt)
end <- as.Date(end,origin="1970-01-01")
# make time interval
int <- interval(strt, end)

dates <- seq(strt[1], end[221], by="day")
dates <- dates[which(month(dates) %in% c(1,2,3,10,11,12))]
#create empty dataframe and name columns appropriately
gscuring <- as.data.table(matrix(0, nrow=nrow(lonlat),ncol=length(dates)))
old <- colnames(gscuring)
setnames(gscuring, old, as.character(dates))

for (i in 1:length(dates)){
j <- which(dates[i] %within% int)
ifelse(any(dates[i] %within% int),
gscuring[,i] <- curing.dt[,j+1,with=FALSE],gscuring[,i] <- NA)
}

gscuring <- cbind("coord"=curing.dt$coord, gscuring)
gscuring <- gather(gscuring, key="date", value="curing", -1)
gscuring$date <- as.Date(gscuring$date)

# save curing dataframe
save(gscuring, file="./robject/gscuring.rda")

load("./robject/dat0.rda")

#filter so only have coordinates cfa have turned out to
gscuring <- filter(gscuring, coord %in% dat0$coord)
gscuring <- filter(gscuring, date %in% dat0$date)

dat0 <- right_join(gscuring, dat0, by=c("coord", "date"))

save(dat0, file="./robject/dat0.rda")

