#### load packages and data ####
library(data.table)
library(dplyr)
library(ggplot2)
library(splines)
library(gam)

load("./robject/dat0.rda")
load("./robject/train.rda")
load("./robject/sub.rda")

#### plot indiv variables ####
dat0$fire <- as.factor(dat0$fire)
newsub$fire <- as.factor(newsub$fire)

# plot temp
ggplot(dat0, aes(x=temp, color=fire))+geom_density()
#ggplot(dat0, aes(x=sqrt(temp), color=fire))+geom_density()
#ggplot(dat0, aes(x=temp^2, color=fire))+geom_density()
#ggplot(dat0, aes(x=log(temp), color=fire))+geom_density()
ggplot(dat0, aes(x=temp, color=fire)) + geom_density() + facet_wrap(~div) + scale_x_sqrt()


# plot RH
ggplot(dat0, aes(x=rh, color=fire))+geom_density()
ggplot(dat0, aes(x=sqrt(rh), color=fire))+geom_density()
#ggplot(dat0, aes(x=rh^2, color=fire))+geom_density()
#ggplot(dat0, aes(x=log(rh), color=fire))+geom_density()
ggplot(dat0, aes(x=rh, color=fire)) + geom_density() + facet_wrap(~div) + scale_x_sqrt()


# plot WS
ggplot(dat0, aes(x=ws, color=fire))+geom_density()
ggplot(dat0, aes(x=sqrt(ws), color=fire))+geom_density()
#ggplot(dat0, aes(x=ws^2, color=fire))+geom_density()
#ggplot(dat0, aes(x=log(ws), color=fire))+geom_density()
ggplot(dat0, aes(x=ws, color=fire)) + geom_density() + facet_wrap(~div) + scale_x_sqrt()


# plot DF
ggplot(dat0, aes(x=df, color=fire))+geom_density()
ggplot(dat0, aes(x=sqrt(df), color=fire))+geom_density()
ggplot(dat0, aes(x=df^2, color=fire))+geom_density()
#ggplot(dat0, aes(x=log(df), color=fire))+geom_density()

# plot FFDI
ggplot(dat0, aes(x=ffdi, color=fire))+geom_density() + scale_x_log10()
ggplot(dat0, aes(x=sqrt(ffdi), color=fire))+geom_density()
ggplot(dat0, aes(x=log(ffdi+1), color=fire))+geom_density()
ggplot(dat0, aes(x=ffdi, color=fire)) + geom_density() + facet_wrap(~div) + scale_x_sqrt()

# plot vapour
ggplot(dat0, aes(x=vapour, color=fire))+geom_density()
ggplot(dat0, aes(x=sqrt(vapour), color=fire))+geom_density()
ggplot(dat0, aes(x=vapour^2, color=fire))+geom_density()
ggplot(dat0, aes(x=log(vapour), color=fire))+geom_density()
ggplot(dat0, aes(x=vapour, color=fire)) + geom_density() + facet_wrap(~div) 


# plot curing
ggplot(dat0, aes(x=curing, color=fire))+geom_density()
ggplot(dat0, aes(x=sqrt(curing), color=fire))+geom_density()
ggplot(dat0, aes(x=curing^2, color=fire))+geom_density()
#ggplot(dat0, aes(x=log(curing), color=fire))+geom_density()
ggplot(dat0, aes(x=curing^2, color=fire)) + geom_density() + facet_wrap(~div) 


# plot rain
ggplot(dat0, aes(x=rain, color=fire))+geom_density() + scale_x_log10()
ggplot(dat0, aes(x=sqrt(rain), color=fire))+geom_density()
ggplot(dat0, aes(x=log(rain), color=fire))+geom_density()
ggplot(dat0, aes(x=rain, color=fire)) + geom_density() + facet_wrap(~div)+scale_x_log10()


qplot(rh, data=dat0, geom="density", colour=as.factor(fire)) + facet_wrap(~div, ncol=3) + scale_x_sqrt()
qplot(rh, data=dat0, geom="density", colour=as.factor(fire)) + facet_grid(ffdi~div) 

qplot(rh, data=dat0, colour=fire, geom="point") 

ggplot(newsub, aes(sqrt(temp), (rh)^2, color=fire)) + geom_density2d() + facet_wrap(~div)

ggplot(newsub, aes(sqrt(temp), sqrt(ws), color=fire)) + geom_density2d() + facet_wrap(~div)

