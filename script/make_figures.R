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
load("./figures/fire_occur.rda")
#mappy stuff
box <- make_bbox(long, lat, fire.dt, f=0.05)
myMap <- get_stamenmap(bbox=box, zoom=6, maptype = "toner", crop=TRUE)
#map of vic
vicmap <- ggmap(myMap,
                extent = "device")
#add in fires!
firemap <- vicmap +geom_point(aes(x=long, y=lat), data=fire.dt, alpha=.5, color="red3")
savepdf("./figures/firemap")
firemap
dev.off()

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

savepdf("./figures/occurence")
g
dev.off()

#load data
load("/Users/ellesaber/Documents/HonsData/dataset.rda")


load("/Users/ellesaber/Documents/HonsData/analysis/silvan.rda")

silv_long <- gather(silvan, key = var, value = val, col=c(4,8:11))
s <- ggplot(silv_long, aes(x=date, y=val)) + geom_line(color="darkblue") + 
  facet_grid(var~., scales="free") + theme_bw() + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Meteorological Variables") + 
  xlab("Date") + 
  ylab("Value")

s
savepdf("figures/silvan")
s
dev.off()


load("/Users/ellesaber/Documents/HonsData/analysis/sub_train.rda")

d <- sub_data.train
#plot ws, temp and fdr
p <- ggplot(d, aes(ws, temp, color=fdr)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")
savepdf("figures/tw_fdr")
p
dev.off()

#plot ws, temp, and fire
f <- ggplot(d, aes(ws, temp, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

savepdf("./figures/tw_fire")
f
dev.off()

#plot curing, vapour pressure and fire
g <- ggplot(d, aes(curing, vap, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

savepdf("./figures/gv_fire")
g
dev.off()

#plot rh, df and fire
h <- ggplot(d, aes(df, rh, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2") 

savepdf("./figures/dh_fire")
h
dev.off()

#plot ffdi curing fire 
i <- ggplot(d, aes(ffdi, curing, color=fire)) + geom_density2d(alpha=.5) +   
  theme(aspect.ratio=1) + facet_wrap(~district, ncol=5) + theme_bw() + 
  scale_color_brewer(palette = "Dark2")

savepdf("./figures/fc_fire")
i
dev.off()

