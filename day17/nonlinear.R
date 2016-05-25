library(tidyr)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(caret)
library(corrplot)
library(rpart)
# library(rattle)
library(randomForest)
library(earth)
library(Cubist)
library(caretEnsemble)

# load data
red_df = read.csv("datasets/winequality-red.csv", sep=";")
white_df = read.csv("datasets/winequality-white.csv", sep=";")

# remove NAs that don't exist
sum(is.na(red_df))
sum(is.na(white_df))
str(red_df)
str(white_df)
silenced = read.delim("datasets/winequality-white.csv", sep=";")
sum(is.na(silenced))

# plot stuff


# this didn't work
# tall_red = tidyr::gather(red_df, property_type, value, fixed.acidity:alcohol)
# base = ggplot(tall_red, aes(color=property_type)) + geom_jitter(aes(x=value, y=quality), alpha=0.3)
# base

# or this
for (p in names(select(white_df, -quality))) {
  print(ggplot(white_df, aes(x=white_df[[p]], y=quality)) 
        + xlab(p) + geom_point() + geom_smooth())
}
# fixedacity has an outlier
# volatileacidity is bumpy
# citric acid very nonlinear, residual sugar too and has outliers
# chlorides nonlinear
# wow just lots of nonlinear stuff

control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit = train(select(white_df, -quality), white_df$quality,
                  trControl=control, method="glmnet", tuneLength=10)

lin_coefs = coef(caret_fit$finalModel, s=caret_fit$bestTune$lambda)
#corrplot(as.matrix(lin_coefs[-1,]), is.corr=FALSE)
# DENSITY IS HIGHLY ANTI-CORRELATED

# linear model RMSE
caret_preds = predict(caret_fit$finalModel, newx = as.matrix(select(white_df, -quality)), s=caret_fit$bestTune$lambda)
caret_RMSE = RMSE(caret_preds, white_df$quality)

# residual plot
qplot(white_df$quality, caret_preds - white_df$quality)

### KNN
# install.packages("kknn")
library("kknn")

# fitting a KNN model
kknn_caret_fit = train(select(white_df, -quality), white_df$quality,
                       trControl=control, method="kknn", tuneLength=10)

# Now to get the RMSE of that
kknn_preds = predict(kknn_caret_fit$finalModel, select(white_df, -quality))
RMSE(kknn_preds, white_df$quality) # 0.4411503, better!
# kknn_caret_fit$results$RMSE, cross validated with different k values
qplot(white_df$quality, kknn_preds - white_df$quality)




#### FITTING REDS KNN MODEL
r_kknn_caret_fit = train(select(red_df, -quality), red_df$quality,
                       trControl=control, method="kknn", tuneLength=10)

# Now to get the RMSE of that
r_kknn_preds = predict(r_kknn_caret_fit$finalModel, select(red_df, -quality))
RMSE(r_kknn_preds, red_df$quality) # 0.5077229
# kknn_caret_fit$results$RMSE, cross validated with different k values

qplot(red_df$quality, r_kknn_preds - red_df$quality)

# random forests
# mtry matters, usually use mtry = floor(sqrt(p)) or mtry=floor(p/3)
# use former when p/3 rounds to 1-2 or p is << n. always wise to try mtry=p

p = ncol(white_df)-1
wpforest = randomForest(quality ~ ., white_df, mtry=p)
wsqrtforest = randomForest(quality ~ ., white_df, mtry=floor(sqrt(p)))

# no dataframe put in does out of bag error
wp_preds = predict(wpforest)
wsqrt_preds = predict(wsqrtforest)

wp_RMSE = caret::RMSE(wp_preds, white_df$quality)
wsqrt_RMSE = caret::RMSE(wsqrt_preds, white_df$quality)
wp_RMSE # 0.5913973
wsqrt_RMSE # 0.5854594
# not bad, not as good as KNN


# gradient-boosted trees
library(gbm)

# getModelInfo("gbm")
control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
gbm_hyperparameters = train(select(white_df, -quality), white_df$quality,
                  trControl=control, method="gbm",
                  tuneGrid = expand.grid(n.trees=500,
                                         shrinkage=10^seq(-3, 0, 1),
                                         interaction.depth=seq(3),
                                         n.minobsinnode=seq(10, 50, 10)))
# The final values used for the model were n.trees = 500,
# interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10. 

gbm_fit = gbm(quality ~ ., data=white_df, n.trees=5000, interaction.depth = 3,
    shrinkage = 0.1, n.minobsinnode = 10, cv.folds = 3)

# min cv RMSE, then plot all cv-ed rmses
min(gbm_fit$cv.error) # 0.4721768, better than KNN even!
qplot(y=gbm_fit$cv.error, x=seq(5000))

# how much does it overfit? time to find out
gbm_preds = predict(gbm_fit, select(white_df, -quality))
RMSE(gbm_preds, white_df$quality) # 0.5312718, so you can see some overfit


# Non-proprietary MARS
control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
mars_hype = train(select(white_df, -quality), white_df$quality,
                  trControl=control, method="earth",
                  tuneGrid = expand.grid(degree=1:5,
                                         nprune=10:20))
# Fitting nprune = 19, degree = 3 on full training set
mars_preds = predict(mars_hype$finalModel)
RMSE(mars_preds, white_df$quality) # 0.7014703 sucks
actual_earth = earth(quality ~ ., degree=3, nprune=19, data=white_df)
print(actual_earth)
summary(actual_earth)

qplot(white_df$quality, mars_preds)

# h = function(v) { max(0, 0.27-v)}


# Non-proprietary Cubist
cubist_hype = train(select(white_df, -quality), white_df$quality,
                    trControl=control, method="cubist",
                    tuneGrid = expand.grid(committees=seq(10,30,5),
                                           neighbors=0:9))

# Fitting committees = 30, neighbors = 6 on full training set
cubist_preds = predict(cubist_hype$finalModel, white_df, neighbors=6)
RMSE(cubist_preds, white_df$quality) # 0.4233416
min(cubist_hype$results$RMSE) # 0.6732227

# aaahhh can't be used for classification :(

# Stacking
ensemble_methods = c("glmnet", "kknn", "rpart")
ensemble_control = trainControl(method="repeatedcv", repeats=1,
                                number=3, verboseIter = TRUE,
                                savePredictions = "final")
ensemble_tunes = list(
  glmnet=caretModelSpec(method="glmnet", tuneLength=10),
  kknn=caretModelSpec(method="kknn", tuneLength=10),
  rpart=caretModelSpec(method="rpart", tuneLength=10)
)

ensemble_fits = caretList(quality ~ ., white_df,
                            trControl=ensemble_control,
                             methodList=ensemble_methods,
                             tuneList=ensemble_tunes)

fit_ensemble = caretEnsemble(ensemble_fits)
print(fit_ensemble)
summary(fit_ensemble)

# BEFORE ADDING GRADIENT BOOST
# The following models were ensembled: glmnet, kknn, rpart, glmnet.1, kknn.1, rpart.1 
# They were weighted: 
#   -0.5622 -0.3375 -0.5549 0.303 0.63 1.1463 -0.0943
# The resulting RMSE is: 0.6713
# The fit for each individual model on the RMSE is: 
#   method      RMSE      RMSESD
# glmnet 0.7531562 0.007753975
# kknn 0.6900136 0.005625101
# rpart 0.7440163 0.005170520
# glmnet.1 0.7531284 0.007730647
# kknn.1 0.6915205 0.006009126
# rpart.1 0.7821552 0.007683435

# Stacking, redux
ensemble_methods = c("glmnet", "kknn", "rpart", "gbm")
ensemble_control = trainControl(method="repeatedcv", repeats=1,
                                number=3, verboseIter = TRUE,
                                savePredictions = "final")
ensemble_tunes = list(
  glmnet=caretModelSpec(method="glmnet", tuneLength=10),
  kknn=caretModelSpec(method="kknn", tuneLength=10),
  rpart=caretModelSpec(method="rpart", tuneLength=10),
  gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees=500,
                                                        shrinkage=10^seq(-3, 0, 1),
                                                        interaction.depth=seq(3),
                                                        n.minobsinnode=seq(10, 50, 10)))
)

ensemble_fits = caretList(quality ~ ., white_df,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes)

fit_ensemble = caretEnsemble(ensemble_fits)
print(fit_ensemble)
summary(fit_ensemble)
# The following models were ensembled: glmnet, kknn, rpart, gbm, glmnet.1, kknn.1, rpart.1, gbm.1 
# They were weighted: 
#   -0.0428 1.3086 -1.4738 0.0717 0.4409 -1.2883 1.8327 -0.1027 0.2166
# The resulting RMSE is: 0.6563
# The fit for each individual model on the RMSE is: 
#   method      RMSE      RMSESD
# glmnet 0.7533464 0.013615293
# kknn 0.6917053 0.012189862
# rpart 0.7460467 0.008810107
# gbm 0.6828631 0.009009401
# glmnet.1 0.7532886 0.013188679
# kknn.1 0.6933058 0.011361976
# rpart.1 0.7826660 0.023430119
# gbm.1 0.6937775 0.006752477

# CARET STACK
gbm_ensemble = caretStack(ensemble_fits, method="gbm", tuneLength=10)
gbm_ens_preds = predict(gbm_ensemble, white_df)
RMSE(gbm_ens_preds, white_df$quality) # 0.5115175

# quite good, quite good
print(gbm_ensemble)
summary(gbm_ensemble)
qplot(white_df$quality, gbm_ens_preds - white_df$quality)
