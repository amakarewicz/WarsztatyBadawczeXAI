library(mlr)
library(dplyr)
library(pROC)

### Preparing data

phones <- read.csv('./phones.csv')
phones[is.na(phones)] <- 0 # NA means phone does not have camera
phones <- phones[phones$back_camera_mpix < 90 &
                   phones$battery_mAh < 7000 &
                   phones$flash_gb < 400 &
                   phones$front_camera_mpix < 40, ]
phones <- phones[, -c(1, 9, 10)] # removing name, height, width beacause they are useless
phones <- phones %>% group_by(brand) %>% mutate(brand = mean(price)) # target encoding

task <- makeRegrTask(id = 'phones', data = phones, target = "price")

# lrns <- as.data.frame(listLearners())
# lrns <- lrns[lrns$type == 'regr', ]

#################################################################################
#################################################################################


library(caret)

model <- caret::train(
  price ~.,
  tuneLength = 1,
  data = phones, 
  method = "ranger",
  trControl = caret::trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = FALSE)
)


#################################################################################
#################################################################################

# Creating model

learner <- makeLearner("regr.ranger")
# , par.vals = list(type = "eps-regression",kernel = "radial",degree = 3)
# getHyperPars(learner)

cv <- makeResampleDesc("CV", iters = 5)
# listMeasures(obj = "regr")
resampled <- resample(learner, task, cv, measures = list(rmse, mae))
resampled$aggr[2]
# resampled$pred$data



