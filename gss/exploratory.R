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
factor_analysis = function(df, num_components, covar=TRUE) {
  factors = fa(df, num_components, rotate="oblimin", fm="pa", covar=covar)
  corrplot(factors$score.cor)
  return(factors)
}

# Nat -> Loadings -> Keys -> Matrix of [Questions], [Codings]
getQuestions = function(n_qs=10, loadings, keys) {
  fact_questions = lapply(loadings, function(c) keys$values[order(abs(c), decreasing=TRUE)][1:n_qs])
  fact_codings = lapply(loadings, function(c) keys$X[order(abs(c), decreasing=TRUE)][1:n_qs])
  return(cbind(fact_questions, fact_codings))
}


### MAIN ### 
df = read.csv("~/repos/signal/gss/gss.csv")
#df = df %>% select(-X, -id, -formwt, -relhh1, -relhhd1, -relhh2, -relhhd2, -oversamp, -sampcode, -year)
#df = df %>% filter(sex == 2)
keys = read.csv("~/repos/signal/gss/keys.csv")
keys = keys[keys$X %in% names(df),]
keys = keys[order(keys$X),]

# Conduct a scree test: 6, 7, 9, or 12
cm = scree(df)
eig_cor_matrix = eigen(cm)
qplot(1:80, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:80])
scree_n = 16

# imputation
fit = softImpute(as.matrix(df), rank.max=16, lambda = 10, type="svd")
ndf = as.data.frame(complete(as.matrix(df), fit))
df = ndf

# re-compute correlation matrix, conduct factor analysis
cm = scree(df)
factors = factor_analysis(cm, scree_n) # side-effect: corrplot(factors$score.cor)

# Grab the related questions for the factors
qs = getQuestions(20, loadings, keys)
qs[,1]

# the rotation matrix; needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree_n])

# compute the scores (change of basis)
scores = as.matrix(df) %*% as.matrix(factors$loadings[,1:scree_n])


# write codings
factor_codings = file("~/repos/signal/gss/factor_codings.txt")
codings = NULL
for (c in names(qs[,2])) { codings = c(codings, c, as.character(unlist(qs[,2][c])), "\n") }
writeLines(codings, con = factor_codings)

factor_questions = file("~/repos/signal/gss/factor_questions.txt")
questions = NULL
for (c in names(qs[,1])) { questions = c(questions, c, as.character(unlist(qs[,1][c])), "\n") }
writeLines(questions, con = factor_questions)

