library(data.table)
library(dplyr)
library(ggplot2)
library(proxy)


#### sampling function #### 
mysample <- function(n,trial=50)
{
  require(proxy)
  trial <- min(trial,nrow(z)-n)
  sampledrows <- rep(FALSE,nrow(z))
  # Start with random row
  sampledrows[sample(nrow(z),1)] <- TRUE
  for(j in 2:n)
  {
    i <- sample(which(!sampledrows),trial)    
    d <- dist(z[i,], z[sampledrows,])
    sampledrows[i[which.max(apply(d,1,min))]] <- TRUE
  }
  return(which(sampledrows))
}


load("./robject/dat0.rda")
set.seed(2311)

#### create test and train #### 

train <- dat0 %>% 
  group_by(subdiv, fire) %>% 
  sample_frac(.8, replace=FALSE)

test <- setdiff(dat0, train)

#### create balanced samples for training set ####




prop.table(table(dat0$fire, dat0$subdiv),2)
prop.table(table(samps$fire, samps$subdiv),2)

x <- filter(dat0, fire==1)
sum(x$fire)      

prop.table(table(dat0$div))

coord <- distinct(data.frame("lat"=dat0$lat, "long"=dat0$long, "div"=dat0$div, "subdiv" = dat0$subdiv))
table(test$div)
table(test$subdiv)
