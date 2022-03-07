#####################################################
####XGBoosting for habitat type######################
#####################################################
rm(list = ls()) # clear working directory
library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("dplyr")    # for some data preperation
library("Ckmeans.1d.dp") # for xgb.ggplot.importance
library(dummies)
setwd("~/Dropbox/Publications_Work/Ecosphere_REV/DATA")

fulldata <- read.csv("fulldata.csv")
subdata <- fulldata[, c("Adjusted_sex",
                        "Distance", 
                        "Distance_to_wetland", 
                        "Distance_to_meadows", 
                        #"Soil_description",
                        "Top_layer", 
                        "Middle_layer", 
                        "Parent_material", 
                        "Drainage", 
                        "Hydric_soil", 
                        "LOCAL_NAME", 
                        #"FORM_NAME", 
                        "ALLIAN_SCI"
                        )]


for(i in c(1:11)){
  subdata[,i] <- as.numeric(subdata[,i]) #make all numeric
}

subdata$Adjusted_sex  <- subdata$Adjusted_sex -1

dat <- subdata

colnames(dat) <- c("Adjusted_sex",
                   "Distance to breeding pool", 
                   "Distance to other wetland", 
                   "Distance to meadow", 
                   #"Soil classification", 
                   "Soil texture (top)", 
                   "Soil texture (middle)", 
                   "Soil parent material", 
                   "Drainage", 
                   "Hydric soil", 
                   "Habitat name",
                   #"Habitat code", 
                   "Vegetation alliance"
                   )




train_index <- sample(1:nrow(dat), nrow(dat)*0.75)
data_variables <- as.matrix(dat[,-1])
data_label <- dat[,"Adjusted_sex"]
data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

numberOfClasses <- length(unique(dat$Adjusted_sex))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
#head(OOF_prediction)


confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
conmat <- confusionMatrix(factor(test_prediction$max_prob),
                          factor(test_prediction$label),
                          mode = "everything")


confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")



names <-  colnames(dat[,-1])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)



gp = xgb.ggplot.importance(importance_matrix)
print(gp) 

names <-  colnames(dat[,-1])
# compute feature importance matrix
importance_matrix <- xgb.importance(feature_names = names, model = bst_model)
#head(importance_matrix)

importance_matrix$accuracy <- conmat$overall[1]
importance_matrix$accuracylower <- conmat$overall[3]
importance_matrix$accuracyupper <- conmat$overall[4]



#Do a quick regression of SVL ~ all thee things 

subF <- subset(fulldata, Adjusted_sex=="NBA")
model <- lm(SVL_mm ~ Distance + Distance_to_wetland + Distance_to_meadows + Top_layer +
              Middle_layer + Parent_material + Drainage + Hydric_soil + FORM_NAME, data=fulldata)

#model <- lm(SVL_mm ~ Distance + Distance_to_wetland + Distance_to_meadows + Soil_description + FORM_NAME, data=fulldata)

summary(model)

"Distance", 
"Distance_to_wetland", 
"Distance_to_meadows", 
"Top_layer", 
"Middle_layer", 
"Parent_material", 
"Drainage", 
"Hydric_soil", 
"LOCAL_NAME", 
"FORM_NAME", 
"ALLIAN_SCI")]




