library("psych")
library("ggplot2")
library("dplyr")
library("corrplot")
library("DAAG")

df = msq
df = df %>% select(Extraversion, Neuroticism, active:scornful)

# removing purely through visual inspection
colSums(is.na(df))
df = df %>% select(-anxious, -cheerful, -idle, -inactive, -alone, -kindly, -scornful, -tranquil)

# remove all rows with any NAs
nonNArows = NULL
for (i in seq(nrow(df))) {
  if (sum(is.na(df[i,])) == 0) {
    nonNArows = c(nonNArows, i)
  }
}

ndf = df[nonNArows,]
extraversion = msq$Extraversion[nonNArows]
neuroticism = msq$Neuroticism[nonNArows]

# Run PCA on remaining variables
p = prcomp(ndf, scale.=TRUE)

# prints the top 10 loadings of the nth principle component
# ordered by absolute value
top = function(n, p) {
  p$rotation[order(abs(p$rotation[,n]), decreasing=TRUE),][1:10,n]
}

# look at the first PCA loadings for the first 5-10 principal components
for (i in seq(10)) {
  print(top(i, p))
}
# PC1 : expressive
# PC2 : positive/negative affect
# PC3 : serenity of mind
# PC4 : energy level
# PC5 : keyed up, arousal
# seems I should take maybe 5 or 7 principal components

# plot with corrplot
corrplot(p$rotation[,1:5], is.corr=FALSE)

# plot eigenvalues
# p$sdev gives the eigenvalues of the covariance matrix of the data. One can
# interpret the n th value in p$sdev as corresponding to the relative propor-
# tion of the variance in the data explained by the n th principal component.
# Put another way, p$sdev[n] / sum(p$sdev) is the proportion of the
# variance in the data explained by the n th principal component.
p$sdev
plot(x=seq(length(p$sdev)), y=p$sdev)
# well, as we learned, higher magnitude corresponds to more interpretability


# CVlm, 5 folds, up to 7 principal components
ext_features = data.frame(cbind(p$x, Extraversion=extraversion))
neur_features = data.frame(cbind(p$x, Neuroticism=neuroticism))

make_formula = function(...) { paste("Extraversion ~ PC", paste(..., sep="PC", collapse="+PC"), sep = "") } 

# collect root mean square errors
ext_rmses = data.frame()
for (n in seq(7)) {
  fit = CVlm(data=ext_features, form.lm = formula(make_formula(n)), m=5)
  rmse = sqrt(mean((fit$Extraversion - fit$Predicted)^2))
  ext_rmses = rbind(ext_rmses, c(n, rmse))
}

names(ext_rmses) = c("n", "rmse")
ext_rmses

#   n rmse
# 1 1 4.10
# 2 2 4.15
# 3 3 4.15
# 4 4 3.99
# 5 5 4.12
# 6 6 4.10
# 7 7 3.82

plot(ext_rmses$n, ext_rmses$rmse)

# It definitely seems that 4 and 7 principal components capture the most
# min RMSE for regularized linear regression was 3.91, mean was 3.98
