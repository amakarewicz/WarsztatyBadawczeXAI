library(mlr)
library(dplyr)
library(pROC)

phones <- read.csv('./phones.csv')
phones[is.na(phones)] <- 0 # NA means phone does not have camera
phones <- phones[, -c(1, 9, 10)] # removing name, height, width beacause they are useless
phones <- phones %>% group_by(brand) %>% mutate(brand = mean(price)) # target encoding

task <- makeRegrTask(id = 'phones', data = phones, target = "price")

# lrns <- as.data.frame(listLearners())
# lrns <- lrns[lrns$type == 'regr', ]
# getLearnerParamSet("regr.svm")

learner <- makeLearner("regr.svm")

cv <- makeResampleDesc("CV", iters = 5)
# listMeasures(obj = "regr")
resampled <- resample(learner, task, cv, measures = list(rmse, mae))
# resampled$aggr
# resampled$pred$data

plot(1:414, resampled$pred$data$truth)
plot(1:414, resampled$pred$data$response)

roc_curve <- roc(resampled$pred$data$truth, resampled$pred$data$response)
plot(roc_curve)
