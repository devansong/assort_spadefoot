#extreme gradient boosting

#raw data
#https://www.fueleconomy.gov/feg/epadata/19data.zip
graphics.off()
rm(list = ls()) # clear working directory
library(xgboost)
library(caret)
library(dummies)
library(DiagrammeR)
library("Ckmeans.1d.dp") # for xgb.ggplot.importance

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")
title <- "extreme gradient boosting"
fulldata <- read.csv("fulldata_withdist.csv")
fulldata <- subset(fulldata, Year == "2017")
subdata <- fulldata[, c("SVL_mm", 
                        "Dist_to_breeding_pool", 
                        "Dist_to_wetland", 
                        "Dist_to_meadow", 
                        "Top_layer", 
                        "Middle_layer", 
                        "Parent_material", 
                        "Drainage", 
                        "Hydric_soil", 
                        "LOCAL_NAME", 
                        "ALLIAN_SCI"
)]

colnames(subdata) <- c("Snout-vent length", 
                       "Distance to breeding pool", 
                       "Distance to other wetland", 
                       "Distance to meadow",
                   "Soil texture (top)", 
                   "Soil texture (middle)", 
                   "Soil parent material", 
                   "Drainage", 
                   "Hydric soil", 
                   "Habitat name",
                   "Vegetation alliance"
)




tmp <- subdata[, c(5:11)]
tmp1 <- dummy.data.frame(tmp)
d <- data.frame(subdata[, c(1:4)], tmp1)
m <- as.matrix(d)

set.seed(123)
indices <- sample(1:nrow(subdata), size = 0.75 * nrow(subdata))
train <- m[indices,]
test <- m[-indices,]

m1_xgb <-
  xgboost(
    data = train[, 2:38],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )

pred_xgb <- predict(m1_xgb, test[, 2:38])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)
abline(lm(yhat ~ y))

#plot first 3 trees of model
xgb.plot.tree(model = m1_xgb, trees = 0:2)

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")

gp = xgb.ggplot.importance(importance_matrix)
print(gp) 

write.csv(importance_matrix, file="importance_matrix_2017_SVL.csv", row.names = FALSE)



#RMSE      Rsquared   MAE
#1.7374    0.8998     1.231

#################
#grid search
#create hyperparameter grid
hyper_grid <- expand.grid(max_depth = seq(3, 6, 1),
                          eta = seq(.2, .35, .01))
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:34],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

#ideal hyperparamters
hyper_grid[which.min(xgb_test_rmse), ]

#max_depth  eta
#6         0.25

###REPEAT FOR 2016 DATA 

graphics.off()
rm(list = ls()) # clear working directory
library(xgboost)
library(caret)
library(dummies)
library(DiagrammeR)
library("Ckmeans.1d.dp") # for xgb.ggplot.importance

setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")
title <- "extreme gradient boosting"
fulldata <- read.csv("fulldata_withdist.csv")
fulldata <- subset(fulldata, Year == "2016")
fulldata <- subset(fulldata, SVL_mm > 1)
subdata <- fulldata[, c("SVL_mm", 
                        "Dist_to_breeding_pool", 
                        "Dist_to_wetland", 
                        "Dist_to_meadow", 
                        "Top_layer", 
                        "Middle_layer", 
                        "Parent_material", 
                        "Drainage", 
                        "Hydric_soil", 
                        "LOCAL_NAME", 
                        "ALLIAN_SCI"
)]

colnames(subdata) <- c("Snout-vent length", 
                       "Distance to breeding pool", 
                       "Distance to other wetland", 
                       "Distance to meadow",
                       "Soil texture (top)", 
                       "Soil texture (middle)", 
                       "Soil parent material", 
                       "Drainage", 
                       "Hydric soil", 
                       "Habitat name",
                       "Vegetation alliance"
)




tmp <- subdata[, c(5:11)]
tmp1 <- dummy.data.frame(tmp)
d <- data.frame(subdata[, c(1:4)], tmp1)
m <- as.matrix(d)

set.seed(123)
indices <- sample(1:nrow(subdata), size = 0.75 * nrow(subdata))
train <- m[indices,]
test <- m[-indices,]

m1_xgb <-
  xgboost(
    data = train[, 2:32],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )

pred_xgb <- predict(m1_xgb, test[, 2:32])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

r <- y - yhat
plot(r, ylab = "residuals", main = title)

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted",
     main = title)
abline(lm(yhat ~ y))

#plot first 3 trees of model
xgb.plot.tree(model = m1_xgb, trees = 0:2)

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")

gp = xgb.ggplot.importance(importance_matrix)
print(gp) 

write.csv(importance_matrix, file="importance_matrix_2016_SVL.csv", row.names = FALSE)











