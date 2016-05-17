library("ggplot2")
library("dplyr")
library("corrplot")
library("DAAG")
library("dummies")
library("pROC")
library("glmnet")

df = read.csv("~/signal/speeddating-aggregated.csv")

activities = df %>% select(sports:yoga)

# drop rows corresponding to people who didn't answer the activities questions
# they all seem fine..?
any(is.na(activities))
check = lapply(activities, factor)

# run PCA on the activity questions
p = prcomp(activities, scale.=TRUE)
summary(p)

# prints the top 10 loadings of the nth principle component
# ordered by absolute value
top = function(n, p) {
  p$rotation[order(abs(p$rotation[,n]), decreasing=TRUE),][1:10,n]
}

# look at the first PCA loadings for the first 5-10 principal components
for (i in seq(5)) {
  print(top(i, p))
}

# PC1 culture/activities
# PC2 competition/watching
# PC3 passiveness of activity
# PC4 lone / self-focused
# PC5 ..? out vs in? people focused?

# plot with corrplot
corrplot(p$rotation[,1:5], is.corr=FALSE)

# plot eigenvalues
p$sdev
plot(x=seq(length(p$sdev)), y=p$sdev)
# by scree test, again maybe 4 or 7 components

# convenience function
make_formula = function(target, ...) { paste(target, " ~ PC", paste(..., sep="PC", collapse="+PC"), sep = "") } 

# Predict gender!
gender = df %>% select(gender)
any(is.na(gender)) #great, FALSE

# prepare dataframe
gender_df = cbind(p$x, gender)

# PC1, then PC1 + PC2, PC1 + PC2 .. PCn .. collect all these logistic regressions
gender_fits = vector(mode="list", length=ncol(activities))
for (n in seq(ncol(activities))) {
  fit = glm(make_formula("gender", seq(n)), family="binomial", data=gender_df)
  gender_fits[[n]] = fit
}

# nooo recycling! but good enough to look at I guess
m = Reduce(cbind, lapply(gender_fits, coef), right=TRUE)
aucs = unlist(lapply(gender_fits, function(x) { auc(roc(x$data$gender, x$fitted.values))[1] }))
# [1] 0.706 0.718 0.724 0.751 0.751 0.764 0.783 0.783 0.783 0.784 0.786 0.807
# [13] 0.822 0.824 0.825 0.825 0.825


# stepwise, cheats as last fit had all principal components
# area here is 0.823
# this seems pretty good in comparison. PCA is for interpretability?
step_model = step(fit, gender ~ .)
auc(roc(gender, step_model$fitted.values))

# regularized linear regression
reg_model = glmnet(p$x, gender[[1]], family="binomial")
cv_model = cv.glmnet(p$x, gender[[1]], family="binomial")
auc(roc(gender[[1]], predict(reg_model, p$x, s=cv_model$lambda.min)[,1])) # 0.825


# Predict race (whites and asians only)
# that's races 2 and 4
races = df %>% select(race)
selector = races == 2 | races == 4
race = dummy(races[selector,])[,2] # convert to binary
races_df = data.frame(cbind(p$x[selector,], race))

races_fits = vector(mode="list", length=ncol(activities))
for (n in seq(ncol(activities))) {
  fit = glm(make_formula("race", seq(n)), family="binomial", data=races_df)
  races_fits[[n]] = fit
}

m = Reduce(cbind, lapply(races_fits, coef), right=TRUE)
aucs = unlist(lapply(races_fits, function(x) { auc(roc(x$data$race, x$fitted.values))[1] }))
# [1] 0.534 0.586 0.654 0.654 0.655 0.666 0.668 0.667 0.683 0.683 0.687 0.687 0.689 0.691 0.691 0.691 0.691

step_model = step(fit, race ~ .)
auc(roc(step_model$data$race, step_model$fitted.values)) #0.678

reg_model = glmnet(p$x[selector,], race, family="binomial")
cv_model = cv.glmnet(p$x[selector,], race, family="binomial")
auc(roc(race, predict(reg_model, p$x[selector,], s=cv_model$lambda.min)[,1])) # 0.683


# Predict career code, restricting to academia and business/finance
# codes 2 and 7
careers = df %>% select(career_c)
selector = careers == 2 | careers == 7
career = dummy(careers[selector,])[,2]
careers_df = data.frame(cbind(p$x[selector,], career))

careers_fits = vector(mode="list", length=ncol(activities))
for (n in seq(ncol(activities))) {
  fit = glm(make_formula("career", seq(n)), family="binomial", data=careers_df)
  careers_fits[[n]] = fit
}

m = Reduce(cbind, lapply(careers_fits, coef), right=TRUE)
aucs = unlist(lapply(career_fits, function(x) { auc(roc(x$data$career, x$fitted.values))[1] }))
# [1] 0.706 0.718 0.724 0.751 0.751 0.764 0.783 0.783 0.783 0.784 0.786 0.807 0.822 0.824 0.825 0.825 0.825


step_model = step(fit, career ~ .)
auc(roc(step_model$data$career, step_model$fitted.values)) #0.75

reg_model = glmnet(p$x[selector,], career, family="binomial")
cv_model = cv.glmnet(p$x[selector,], career, family="binomial")
auc(roc(career, predict(reg_model, p$x[selector,], s=cv_model$lambda.min)[,1])) # 0.741
