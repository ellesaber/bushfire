library(data.table)
library(dplyr)
library(ggplot2)
library(proxy)
library(lubridate)

#### sampling function #### 
mysample <- function(z,trial=50)
{
  #require(proxy)
  n <- sum(z$fire)
  z.fire <- z %>% filter(fire == 1)
  z <- z %>% filter(fire == 0)
  z.s <- data.frame(date=scale(z$date), lat=scale(z$lat), long=scale(z$long))
  trial <- min(trial,nrow(z)-n)
  sampledrows <- rep(FALSE, nrow(z))
  # Start with random row
  sampledrows[sample(nrow(z),1)] <- TRUE
  for(j in 2:n)
  {
    i <- sample(which(!sampledrows),trial)    
    d <- dist(z.s[i,], z.s[sampledrows,])
    sampledrows[i[which.max(apply(d,1,min))]] <- TRUE
  }
  z.new <- rbind(z.fire, z[which(sampledrows),]) 
  cat(nrow(z.new), " ", nrow(z), "\n")
  return(z.new)
}

load("./robject/dat0.rda")
set.seed(2311)

#### Add in fire danger rating to dataset #### 

# vec contains intervals for fire danger ratings
vec <- c(0, 12, 25, 50, 75, 100)
c <- data.table("fdr"=findInterval(dat0$ffdi, vec))

dat0$fdr = factor(c$fdr, labels=c("lowmod", "high", "vhigh", "severe", "extreme", "codered"), ordered = TRUE)

dat.sub <- dat0 %>% select(date, lat, long, div, fire, curing, temp, ffdi, fdr, df, rh, ws, vapour, rain)

dat.sub.2001 <- dat.sub %>% filter(year(date) == "2001")
table(dat.sub.2001$div)
prop.table(table(dat.sub.2001$div, dat.sub.2001$fire),1)
dat.sub.2001.barwon <- dat.sub.2001 %>% filter(div == "Barwon")
dat.sub.2001.barwon$date <- as.numeric(dat.sub.2001.barwon$date-min(dat.sub.2001.barwon$date))
newsub <- mysample(dat.sub.2001.barwon)
dat.sub.2001$date <- as.numeric(dat.sub.2001$date-min(dat.sub.2001$date))
newsub <- dat.sub.2001 %>% group_by(div) %>% mysample
newsub <- dat.sub.2001 %>% filter(div == "Central Highlands") %>% mysample

newsub <- dat.sub.2001 %>% group_by(div) %>% do(mysample(.))


#### create test and train #### 

train <- dat.sub %>% 
  group_by(div, fire) %>% 
  sample_frac(.8, replace=FALSE)

test <- setdiff(dat0, train)

#### create balanced samples for training set ####
# use just one year while making function work

train$date <- as.numeric(train$date-min(train$date))

newsub <- train %>% group_by(div) %>% do(mysample(.))

table(newsub$div, newsub$fire)
summary(newsub)
ggplot(newsub, aes(x=long, y=lat, colour=div)) + geom_point()

ggplot(newsub, aes(x=date, y=lat, colour=div)) + geom_point()


qplot(rh, data=dat0, geom="density") + facet_wrap(~fire, ncol=1) + scale_x_sqrt()
qplot(rh, data=dat0, geom="density", colour=as.factor(fire)) + facet_wrap(~div, ncol=3) + scale_x_sqrt()
qplot(rh, data=dat0, geom="density", colour=as.factor(fire)) + facet_grid(ffdi~div) 

qplot(rh, data=dat0, colour=fire, geom="point") 




