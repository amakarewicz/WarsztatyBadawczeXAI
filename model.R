library(mlr)
library(dplyr)
library(pROC)

### Preparing data

phones <- read.csv('./phones.csv')
phones[is.na(phones)] <- 0 # NA means phone does not have camera
phones <- phones[, -c(1, 9, 10)] # removing name, height, width beacause they are useless
phones <- phones %>% group_by(brand) %>% mutate(brand = mean(price)) # target encoding

task <- makeRegrTask(id = 'phones', data = phones, target = "price")

# lrns <- as.data.frame(listLearners())
# lrns <- lrns[lrns$type == 'regr', ]

#################################################################################
#################################################################################

### Testing hyper parameteres

# getLearnerParamSet("regr.svm")

kernel <- c('linear', 'polynomial', 'radial', 'sigmoid')

for (ker in kernel) {
  learner <- makeLearner("regr.svm", par.vals = list(kernel = ker))
  cv <- makeResampleDesc("CV", iters = 5)
  resampled <- resample(learner, task, cv, measures = list(rmse, mae))
  print(ker)
  print(resampled$aggr)
}
# radial is the best kernel

###########################

types <- c('eps-regression', 'nu-regression')

eps <- c()
nu <- c()
for (i in 1:50) {
  learner <- makeLearner("regr.svm", par.vals = list(type = 'eps-regression'))
  cv <- makeResampleDesc("CV", iters = 5)
  resampled <- resample(learner, task, cv, measures = list(rmse, mae))
  eps[i] <- resampled$aggr
  
  learner <- makeLearner("regr.svm", par.vals = list(type = 'nu-regression'))
  cv <- makeResampleDesc("CV", iters = 5)
  resampled <- resample(learner, task, cv, measures = list(rmse, mae))
  nu[i] <- resampled$aggr
}
summary(eps)
summary(nu)
# eps-regression is the best type

#################################

degrees <- data.frame(matrix(, nrow = 10, ncol = 10))
for (i in 1:10) {
  record <- c()
  for (j in 1:10) {
    learner <- makeLearner("regr.svm", par.vals = list(degree = i))
    cv <- makeResampleDesc("CV", iters = 5)
    resampled <- resample(learner, task, cv, measures = list(rmse, mae))
    record[j] <- resampled$aggr
  }
  degrees[i, ] <- record
}
summary(degrees)
# degree = 3 is the best from 1 - 10 range

#################################################################################
#################################################################################

# Creating model

learner <- makeLearner("regr.svm", par.vals = list(type = "eps-regression",
                                                   kernel = "radial",
                                                   degree = 3))
# getHyperPars(learner)

cv <- makeResampleDesc("CV", iters = 5)
# listMeasures(obj = "regr")
resampled <- resample(learner, task, cv, measures = list(rmse, mae))
resampled$aggr
# resampled$pred$data


# plot(1:414, resampled$pred$data$truth)
# plot(1:414, resampled$pred$data$response)

# roc_curve <- roc(resampled$pred$data$truth, resampled$pred$data$response)
# plot(roc_curve)
