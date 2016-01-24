library(caret)
library(splines)
library(data.table)

load("./robject/train.rda")
load("./robject/sub.rda")
load("./robject/test.rda")


dat0$fire <- as.factor(dat0$fire)
newsub$fire <- as.factor(newsub$fire)

#### Standard linear logits ####
#"cv" = cross-validation, 10-fold
set.seed(2311)
tc <- trainControl("cv", 10, savePredictions=T)  

logit1 <- train(fire~ffdi,
                data      = newsub    ,
                method    = "glm"    ,
                family    = binomial ,
                trControl = tc)

logit2 <- train(fire ~ temp + ffdi + df + rh + ws,
                data      = newsub    ,
                method    = "glm"    ,
                family    = binomial ,
                trControl = tc)
logit3 <- train(fire ~ temp + ffdi + df + rh + ws + curing + vapour + rain,
                data      = newsub    ,
                method    = "glm"    ,
                family    = binomial ,
                trControl = tc)

models <- newsub %>% group_by(div) %>% do(mod = train(fire ~ temp + ffdi + df + rh + ws,
                                                      data      = .    ,
                                                      method    = "glm"    ,
                                                      family    = binomial ,
                                                      trControl = tc))
class(models$mod)
lapply(models$mod, predict, )