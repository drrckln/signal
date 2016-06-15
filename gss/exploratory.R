library("corrplot")
library("dplyr")
library("ggplot2")
library("psych")
library("softImpute")

# fix keys
# remove qs
# softImpute
# superfactor?
# rank 12, lambda 7 or 8

# DataFrame -> CorrelationMatrix
# side-effect: scree plot
scree = function(df) {
  cor_matrix = cor(df, use="pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] = 0
  return(cor_matrix)
}

# CorrelationMatrix -> Nat -> FactorAnalysis
# side-effect: corrplot of scores
factor_analysis = function(c_matrix, num_components) {
  fa_df = fa(c_matrix, num_components, rotate="oblimin", fm="pa", covar=TRUE)
  corrplot(fa_df$score.cor)
  return(fa_df)
}

# Nat -> Loadings -> Keys -> Matrix of [Questions], [Codings]
getQuestions = function(n_qs=10, loadings, keys) {
  fact_questions = lapply(loadings, function(c) keys$values[order(abs(c), decreasing=TRUE)][1:n_qs])
  fact_codings = lapply(loadings, function(c) keys$X[order(abs(c), decreasing=TRUE)][1:n_qs])
  return(cbind(fact_questions, fact_codings))
}


### MAIN ### 
df = read.csv("~/repos/signal/gss/gss_old.csv")
df = df %>% select(-X, -id, -formwt, -relhh1, -relhhd1, -relhh2, -relhhd2, -oversamp, -sampcode, -year)
df = df %>% filter(sex == 2)
keys = read.csv("~/repos/signal/gss/keys.csv")
keys = keys[keys$X %in% names(df),]
keys = keys[order(keys$X),]

# Conduct a scree test: 6, 7, 9, or 12
cm = scree(df)
eig_cor_matrix = eigen(cm)
qplot(1:80, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:80])
scree = 14

factors = factor_analysis(cm, scree)
qplot(1:12, eigen(factors$score.cor))

# the rotation matrix; needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree])

qs = getQuestions(10, loadings, keys)
qs[,1]

# imputation
fit = softImpute(as.matrix(df), rank.max=15, lambda = 9, type="svd")
ndf = as.data.frame(complete(as.matrix(df), fit))
df = ndf

