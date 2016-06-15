library("corrplot")
library("dplyr")
library("ggplot2")
library("psych")
library("softImpute")

df = read.csv("~/repos/signal/gss/gss.csv")
#sels = select(df, -X, -year, -oversamp, -formwt, -relate1, -relhhd1, -relhh1)
keys = read.csv("~/repos/signal/gss/keys.csv")
keys = keys[keys$X %in% names(df),]
keys = keys[order(keys$X),]

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
  
  eig_cor_matrix = eigen(cor_matrix)
  qplot(1:80, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:80])
  return(cor_matrix)
}

cm = scree(df)
#eig_cor_matrix = eigen(cm)
#qplot(1:80, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:80])

# Conduct a scree test: 6, 7, 9, or 12
scree = 12

# CorrelationMatrix -> Nat -> FactorAnalysis
# side-effect: corrplot of scores
factor_analysis = function(c_matrix, num_components) {
  fa_df = fa(c_matrix, num_components, rotate="oblimin", fm="pa", covar=TRUE)
  corrplot(fa_df$score.cor)
  return(fa_df)
}

factors = factor_analysis(cm, scree)

# qplot(1:12, eigen(fa_df$score.cor))


# the rotation matrix needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree])

# Nat -> Loadings -> Keys -> Matrix of [Questions], [Codings]
getQuestions = function(n_qs=10, loadings, keys) {
  fact_questions = lapply(loadings, function(c) keys$values[order(abs(c), decreasing=TRUE)][1:n_qs])
  fact_codings = lapply(loadings, function(c) keys$X[order(abs(c), decreasing=TRUE)][1:n_qs])
  return(cbind(fact_questions, fact_codings))
}

qs = getQuestions(10, loadings, keys)
