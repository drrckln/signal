library("ggplot2")
library("dplyr")
library("glmnet")
library("psych")
library("corrplot")

# Part 1: Regularization
df = msq

# impute with column averages. why didn't the clever way work?
# c[is.na(c)] = mean(c, na.rm=TRUE) replaced whole column with single value
# instead of just the NAs replaced
df = data.frame(lapply(df, function(c) {
  avg = mean(c, na.rm=TRUE)
  sapply(c, function(v) {
    if (is.na(v)) avg else v
  })
}))


extraversion = df$Extraversion
neuroticism = df$Neuroticism
features = df %>% select(active:scornful)


set.seed(1)
alphas = seq(0, 1, 0.1)
lambdas = 10^(seq(1, -3, length.out = 50))

# partition feature rows into n disjoint subsets
# each row in one and only one subset
# generates a vector indicating row mappings to subsets
partition = function(df, n_folds) {
  mappings = sample(nrow(df)) %% n_folds + 1
}

mappings = partition(df, 10)
  
train_sets = vector(mode="list", length=10)
test_sets = vector(mode="list", length=10)
extr_sets = vector(mode="list", length=10)
neur_sets = vector(mode="list", length=10)
  
for (i in 1:10) {
  # Get subsets of the data
  train_sets[[i]] = scale(features[mappings != i, ])
  test_sets[[i]] = scale(features[mappings == i, ], center=attr(train_sets[[i]], "scaled:center"), scale=attr(train_sets[[i]], "scaled:scale"))
  extr_sets[[i]] = extraversion[mappings != i] # training ON
  neur_sets[[i]] = neuroticism[mappings != i]
}

# will eventually hold:
# alpha, lambda, cv.rmse(Extraversion), cv.rmse(Neuroticism)
results = data.frame()

# Vector(numeric) -> Vector(numeric) -> Real
rmse = function(x, y) {
  sqrt(mean((x - y) ^ 2))
}

for (alpha in alphas) {
  print(paste("alpha:", alphas[i]))
  
  e_fits = vector(mode="list", length=10)
  n_fits = vector(mode="list", length=10)
  
  for (i in seq(10)) {
    e_fit = glmnet(train_sets[[i]], extr_sets[[i]], lambda=lambdas, alpha=alpha)
    n_fit = glmnet(train_sets[[i]], neur_sets[[i]], lambda=lambdas, alpha=alpha)
    e_fits[[i]] = e_fit
    n_fits[[i]] = n_fit
  }
  
  
  for (lambda in lambdas) {
    print(paste("lambda:", lambda))
    
    e_predicted = vector(length=nrow(features))
    n_predicted = vector(length=nrow(features))
    
    for (i in seq(10)) {
      e_predicted[mappings == i] = predict(e_fits[[i]], test_sets[[i]], s=lambda)
      n_predicted[mappings == i] = predict(n_fits[[i]], test_sets[[i]], s=lambda)
    }
    
    e_rmse = rmse(e_predicted, extraversion)
    n_rmse = rmse(n_predicted, neuroticism)
    
    results = rbind(results, c(alpha, lambda, e_rmse, n_rmse))
  }
}

colnames(results) = c("alpha", "lambda", "e_rmse", "n_rmse")
print(results)

# convenience function
arg_min = function(v) { match(min(v), v)[1] }

# select the lowest rmse from {alphas} X {lambdas}
e_row = results[arg_min(results$e_rmse),]
n_row = results[arg_min(results$n_rmse),]

# optimal models
bestModelE = glmnet(scale(features), extraversion, alpha=e_row$alpha, lambda=e_row$lambda)
bestModelN = glmnet(scale(features), neuroticism, alpha=n_row$alpha, lambda=n_row$lambda)

# grab the coefficients
e_coefs = coef(bestModelE, s=e_row$lambda)
n_coefs = coef(bestModelN, s=n_row$lambda)

# cbind into matrix, name rows and columns
coeffs = as.matrix(cbind(as.numeric(e_coefs), as.numeric(n_coefs)))
colnames(coeffs) = c("Extraversion", "Neuroticism")
rownames(coeffs) = rownames(e_coefs)

# remove intercept row, remove zeros before quantiling
coeffs = coeffs[rowSums(abs(coeffs)) > 0, ]
coeffs = coeffs[-1,]

# check quantiles
e_75th = quantile(abs(coeffs[,1]))[4]
# 0%         25%         50%         75%        100% 
# -0.25555419 -0.04448161  0.00000000  0.05107396  0.56926763 

n_75th = quantile(abs(coeffs[,2]))[4]
# 0%          25%          50%          75%         100% 
# -0.710583935 -0.008000113  0.000000000  0.048757199  0.428528975 

# remove rows where both coefficients are below 75th percentile
# filter(as.data.frame(coeffs), Extraversion > 0.05107396 | Neuroticism > 0.048757199) # crap no more row names

row_selection = as.logical(Map(function(e, n) { abs(e) >= e_75th | abs(n) >= n_75th }, coeffs[,1], coeffs[,2]))
best_coeffs = coeffs[row_selection,] # SO BADASS

# corrplot
corrplot(best_coeffs, is.corr=FALSE)
