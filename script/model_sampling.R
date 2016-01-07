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

asfunc <- function(x, n){
  x %>% 
    select(day, long, lat) %>% 
    do(mysample(.,n))
}


load("./robject/dat0.rda")
set.seed(2311)

dat.sub <- dat0 %>% select(date, lat, long, div, fire, curing, temp, ffdi, df, rh, ws, vapour, rain)
dat.sub.2001 <- dat.sub %>% filter(year(date) == "2001")
table(dat.sub.2001$div)
prop.table(table(dat.sub.2001$div, dat.sub.2001$fire),1)
dat.sub.2001.barwon <- dat.sub.2001 %>% filter(div == "Barwon")
dat.sub.2001.barwon$date <- as.numeric(dat.sub.2001.barwon$date-min(dat.sub.2001.barwon$date))
newsub <- mysample(dat.sub.2001.barwon)
dat.sub.2001$date <- as.numeric(dat.sub.2001$date-min(dat.sub.2001$date))
newsub <- dat.sub.2001 %>% group_by(div) %>% mysample
newsub <- dat.sub.2001 %>% filter(div == "Central Highlands") %>% mysample

#### create test and train #### 

train <- dat0 %>% 
  group_by(subdiv, fire) %>% 
  sample_frac(.8, replace=FALSE)

test <- setdiff(dat0, train)

#### create balanced samples for training set ####
# use just one year while making function work


# create small samples to work with

x <- filter(train, fire==0 & year(date)==2001)
x$day <- as.numeric(x$date)
x <- ungroup(x)
x <- select(x, div, day, long, lat)

# list of sample sizes
l <- c(11:21)


x$day <- as.numeric(x$date)

# creates samples all of the same size from each district
samps <- x %>% group_by(div) %>% 
  select(day, long, lat) %>% 
  do(mysample(.,5))

#split into list of dataframes by div
sp <- split(x, f=x$div)

# use function on list of dataframes and list of sample sizes
fil <- mapply(asfunc, sp, l)

fil <- x %>% 
    group_by(div) %>% 
    do(asfunc)


# transpose
fil <- t(fil)
# turn each row into a data frame : list of dataframes
ts <- apply(fil, 1, as.data.frame)
# rbinds each dataframe in the list of data frames
df <- do.call("rbind", ts)


