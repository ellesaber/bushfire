#load relevant packages
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(data.table)
library(ggmap)
library(dplyr)
library(tidyr)
library(gridExtra)

# still need to update references to robjects #

#Save PDF without whitespace
savepdf <- function(file, width=16, height=10)
{
  ## This function saves images nicely without whitespace
  .._fname <- paste(file,".pdf",sep="")
  pdf(.._fname, width=width/2.54, height=height/2.54, pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}


#datset
# Fig 7
load("./robject/firedt.rda")

#mappy stuff
box <- make_bbox(long, lat, fire.dt, f=0.05)
myMap <- get_stamenmap(bbox=box, zoom=6, maptype = "toner", crop=TRUE)
myMap <- get_googlemap(center=c(lon=144.5,lat=-37.5), zoom=6, maptype = "roadmap", crop=TRUE, color="bw")
#map of vic
vicmap <- ggmap(myMap,
                extent = "device")
#add in fires!
firemap <- vicmap + geom_point(aes(x=long, y=lat), data=fire.dt, alpha=.1, color="red3") 
#savepdf("./figures/firemap")
firemap 

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "SD11aAust"  
# Read in the data
aus.shape <- readOGR(dsn="./data_raw/shps", layer=layerName) 
#remove other polygons to leave only Victoria
vic.shp <- aus.shape[aus.shape$STATE_CODE == 2,]

devtools::install_github("ateucher/rmapshaper")
library(rmapshaper)
vic.shp.small <- ms_simplify(vic.shp, keep=0.05) # use instead of thinnedSpatialPoly
vic_map <- ggplot2::fortify(vic.shp.small)
ggplot(vic_map, aes(x=long, y=lat, order=order, group=group)) + geom_path()

library(dplyr)
library(purrr)
polys <- as(vic.shp.small, "SpatialPolygons")
class(polys) # should be SpatialPolygons
length(polys) # should be 150

centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
head(centroids)
centroids$id <- vic.shp.small@data$SD_CODE11
centroids$name <- vic.shp.small@data$SD_NAME11

# Make it look like a map
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "white", size=1, fill=NA)

ggplot(vic_map, aes(x=long, y=lat)) + geom_path(aes(order=order, group=group)) +
  geom_point(data=fire.dt, aes(x=long, y=lat), alpha=.1, color="red3") +
  geom_text(data=centroids, aes(x=long_c, y=lat_c, label=name)) + 
  coord_equal() + theme_map

#dev.off()

# Fig 8
f <- data.frame("date"=fire.dt$date)
f$year <- year(f$date)
f$week <- week(f$date)
f$month <- month(f$date)
f$Season <- factor(ifelse(f$month %in% c(4, 5, 6, 7, 8, 9), 
                          "Outisde Fire Season", "Fire Season"), ordered=TRUE)

g <- ggplot(f, aes(date, fill=Season)) + geom_histogram(binwidth=14) + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Count of fires by fortnight") + 
  xlab("Date") + 
  ylab("Number of fires")

#savepdf("./figures/occurence")
g
#dev.off()

#load data
#load("/Users/ellesaber/Documents/HonsData/dataset.rda")

# Figure 2
load("./robject/dtfull.rda")

silvan <- dt.full %>% filter(((LONG-145.42)^2+(LAT+37.82)^2)<0.001) %>% select(date, temp, ffdi, df, rh, ws)
silvan$date <- as.Date("01/01/2000",format="%d/%m/%Y") + days(silvan$date-1)

silv_long <- gather(silvan, key = var, value = val, -date)
s <- ggplot(silv_long, aes(x=date, y=val)) + geom_line(color="darkblue") + 
  facet_grid(var~., scales="free") + theme_bw() + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Meteorological Variables") + 
  xlab("Date") + 
  ylab("Value")

s
#savepdf("figures/silvan")
s
#dev.off()

# Figure 3 - Need to change color scheme to match fire danger rating colors
# Need to run sampling code from model_sampling.R, ro create data for these plots
load("./robject/sub.rda")
load("./robject/train.rda")

d <- newsub
d <- train
#plot ws, temp and fdr
p <- ggplot(d, aes(ws, temp, color=fdr)) + 
  #geom_point(alpha=0.1) + 
  geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~div, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")
#savepdf("figures/tw_fdr")
p
#dev.off()

# Figure 9
#plot ws, temp, and fire
d$fire <- factor(d$fire)
f <- ggplot(d, aes(ws, temp, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~div, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

#savepdf("./figures/tw_fire")
f
#dev.off()

# Fig 11
#plot curing, vapour pressure and fire
g <- ggplot(d, aes(curing, vapour, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~div, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

#savepdf("./figures/gv_fire")
g
#dev.off()

# Fig 10
#plot rh, df and fire
h <- ggplot(d, aes(df, rh, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~div, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2") 

#savepdf("./figures/dh_fire")
h
#dev.off()

# Note sure what this is
#plot ffdi curing fire 
i <- ggplot(d, aes(ffdi, curing, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

savepdf("./figures/fc_fire")
i
dev.off()

