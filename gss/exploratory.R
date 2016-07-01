library("corrplot")
library("dplyr")
library("ggplot2")
library("psych")
library("softImpute")

# DataFrame -> CorrelationMatrix
scree = function(d_f) {
  cor_matrix = cor(d_f, use="pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] = 0
  return(cor_matrix)
}

# CorrelationMatrix -> Nat -> FactorAnalysis
# side-effect: corrplot of scores
factor_analysis = function(d_f, num_components, covar=TRUE) {
  factors = fa(d_f, num_components, rotate="oblimin", fm="pa", covar=covar)
  corrplot(factors$score.cor)
  return(factors)
}

# Nat -> Loadings -> Keys -> Matrix of [Questions], [Codings]
getQuestions = function(n_qs=10, loadings, keys) {
  fact_questions = lapply(loadings, function(c) as.character(keys$values[order(abs(c), decreasing=TRUE)][1:n_qs]))
  fact_codings = lapply(loadings, function(c) as.character(keys$X[order(abs(c), decreasing=TRUE)][1:n_qs]))
  return(cbind(fact_questions, fact_codings))
}

# initial loading
df = read.csv("~/repos/signal/gss/gss.csv")
keys = read.csv("~/repos/signal/gss/keys.csv")
keys = keys[keys$X %in% names(df),]
keys = keys[order(keys$X),]

# Conduct a scree test:
cm = scree(df)
eig_cor_matrix = eigen(cm)
qplot(1:50, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:50], ylab = "Eigenvalue")
scree_n = 16
factors = factor_analysis(cm, scree_n) # side-effect: corrplot(factors$score.cor)

# the rotation matrix; needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree_n])

# Grab the related questions for the factors
qs = getQuestions(20, loadings, keys)
qs[,1][1:6]

# approx equal amounts
male = df$sex == 1
female = df$sex == 2

# sexual orientation
homo = df$sexornt == 1
bi = df$sexornt == 2
hetero = df$sexornt == 3

# race
white = df$race == 1
black = df$race == 2
other_ethnicity = df$race == 3

# more of a power law
protestant = df$relig == 1
catholic = df$relig == 2
jewish = df$relig == 3 #40
ath_agnostic = df$relig == 4
other = df$relig == 5 #27
buddhist = df$relig == 6 #26
hindu = df$relig == 7 #13
other_eastern = df$relig == 8 #3
orthodox_christian = df$relig == 10 #9
christian = df$relig == 11

# religious intensity. 4 is None
strong = df$reliten == 1
not_very_strong = df$reliten == 2
somewhat_strong = df$reliten == 3
no_religion = df$reliten == 4

# work in government or private sector
govt = df$wrkgovt == 1
private = df$wrkgovt == 2

# self-identified class
lower_class = df$class == 1
working_class = df$class == 2
middle_class = df$class == 3
upper_class = df$class == 4 #66

# only selected a few
managers = df$occ10 %in% c(10, 20, 40, 50, 60, 100, 110, 120, 135, 136, 137, 140, 150, 160, 205, 220, 230, 300, 310, 330, 340, 350, 360, 400, 410, 420, 430)
developers = df$occ10 %in% c(1010, 1020, 1030)
engineers = df$occ10 %in% c(1320, 1360, 1400, 1410, 1430, 1450, 1460, 1510, 1530)
scientists = df$occ10 %in% c(1600, 1610, 1640, 1650, 1710, 1720, 1740, 1760)
social_scientists = df$occ10 %in% c(1800, 1820, 1860)
lawyers = df$occ10 == 2100
legal = df$occ10 %in% c(2100, 2105, 2110, 2145, 2160)
doctors = df$occ10 == 3060
medical = df$occ10 %in% c(3010, 3030, 3040, 3050, 3060, 3110, 3160, 3200, 3220, 3255, 3258, 3400, 3600, 3310)
truckers = df$occ10 == 9130
military = df$occ10 %in% c(9800, 9810, 9820, 9830)
teachers = df$occ10 %in% c(2200, 2300, 2310, 2320, 2330, 2340)

# age
under_thirty = df$age < 30
thirty_to_sixty = df$age >= 30 & df$age < 60
sixty_plus = df$age >= 60

# politics
strong_democrat = df$partyid == 0
not_strong_democrat = df$partyid == 1
ind_near_democrat = df$partyid == 2
independent = df$partyid == 3
ind_near_republican = df$partyid == 4
not_strong_republican = df$partyid == 5
strong_republican = df$partyid == 6
other_politics = df$partyid == 7


f = factor(df$partyid, levels = 0:6, labels = c("strong_democrat", "not_strong_democrat", "ind_near_democrat", "independent", "ind_near_republican", "not_strong_republican", "strong_republican"))
f == "strong_democrat"
as.numeric(f)
cor.test
f %in% c("strong_democrat", "not_strong_democrat")
f == "strong_democrat" & age  == "under_thirty"

# imputation
fit = softImpute(as.matrix(df), rank.max=16, lambda = 10, type="svd")
ndf = as.data.frame(complete(as.matrix(df), fit))

# re-compute correlation matrix, conduct factor analysis
cm = scree(ndf)
factors = factor_analysis(cm, scree_n) # side-effect: corrplot(factors$score.cor)

# the rotation matrix; needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree_n])

# compute the scores (change of basis)
scores = as.matrix(ndf) %*% as.matrix(factors$loadings[,1:scree_n])

qplot(scale(scores[,1])) # THAT'S NOT GOOD, breaks assumptions. Confounds.

freedom_questions = c("spkhomo", "libhomo", "librac", "spkrac", "colmslm", "libmslm", "spkmslm", "life", "sprtprsn", "colrac", "comprend", "colcom", "colhomo", "libcom", "spkcom", "libmil", "spkmil", "colmil", "trust") # definitely there are things unrelated to freedom of speech

for (q in freedom_questions) {
  print(qplot(df[social_scientists, q], xlab=q))
}

# must preserve the ordering in the keys!
civil_liberties_qs = c("colath", "colcom", "colhomo", "colmil", "colmslm", "colrac",
                       "libath", "libcom", "libhomo", "libmil", "libmslm", "librac",
                       "spkath", "spkcom", "spkhomo", "spkmil", "spkmslm", "spkrac")

# let's do some additional cleaning as well
df = read.csv("~/repos/signal/gss/gss.csv")

df$colath = df$colath - 4
df$colcom = 5 - df$colcom
df$colhomo = df$colhomo - 4
df$colmil = df$colmil - 4
df$colmslm = df$colmslm - 4
df$colrac = df$colrac - 4

df$libath = 2 - df$libath
df$libcom = 2 - df$libcom
df$libhomo = 2 - df$libhomo
df$libmil = 2 - df$libmil
df$libmslm = 2 - df$libmslm
df$librac = 2 - df$librac

df$spkath = df$spkath - 1
df$spkcom = df$spkcom - 1
df$spkhomo = df$spkhomo - 1
df$spkmil = df$spkmil - 1
df$spkmslm = df$spkmslm - 1
df$spkrac = df$spkrac - 1

# Re-impute and select only the civil liberties questions
fit = softImpute(as.matrix(df), rank.max=16, lambda = 10, type="svd") # maybe I shouldn't re-impute?
ndf = as.data.frame(complete(as.matrix(df), fit))
cl_df = select(ndf, one_of(civil_liberties_qs))
keys = keys[keys$X %in% names(cl_df),]
keys = keys[order(keys$X),]

# re-compute correlation matrix, conduct factor analysis
cm = scree(cl_df)
eig_cor_matrix = eigen(cm)
qplot(1:10, eig_cor_matrix$values[order(abs(eig_cor_matrix$values), decreasing=TRUE)][1:10], ylab = "Eigenvalue") # maybe 9?
scree_n = 1 # try 1-7

factors = fa(cm, 1, covar=TRUE) # side-effect: corrplot(factors$score.cor)
corrplot(factors$score.cor) # shows it is 1 factor

# the rotation matrix; needed after imputation
loadings = as.data.frame(factors$loadings[,1:scree_n])

# Grab the related questions for the factors
qs = getQuestions(10, loadings, keys)
qs[,1] # doesn't look good

# # compute the scores (change of basis)
scores = as.matrix(cl_df) %*% as.matrix(factors$loadings[,1:scree_n])

# aggregate by different categories?
factors$loadings
ndf = ndf %>% select(relig, sex, sexornt, reliten, class, wrkgovt, partyid)
ndf = cbind(ndf, scores)

aggregate(ndf, by=ndf["relig"], FUN=mean)
aggregate(ndf, by=ndf["sex"], FUN=mean)
aggregate(ndf, by=ndf["reliten"], FUN=mean)
aggregate(ndf, by=ndf["class"], FUN=mean)
aggregate(ndf, by=ndf["wrkgovt"], FUN=mean)
aggregate(ndf, by=ndf["partyid"], FUN=mean)
qplot(ndf[managers,"scores"])
qplot(ndf[engineers,"scores"])
qplot(ndf[scientists,"scores"])
qplot(ndf[social_scientists,"scores"])
qplot(ndf[lawyers,"scores"])
qplot(ndf[legal,"scores"])
qplot(ndf[doctors,"scores"])
qplot(ndf[medical,"scores"])
qplot(ndf[truckers,"scores"])
qplot(ndf[military,"scores"])
qplot(ndf[teachers,"scores"])
qplot(ndf[under_thirty,"scores"])
qplot(ndf[thirty_to_sixty,"scores"])
qplot(ndf[sixty_plus,"scores"])

mean(ndf[managers,"scores"])
mean(ndf[engineers,"scores"])
mean(ndf[scientists,"scores"])
mean(ndf[social_scientists,"scores"])
mean(ndf[lawyers,"scores"], na.rm=TRUE)
mean(ndf[legal,"scores"])
mean(ndf[doctors,"scores"], na.rm=TRUE)
mean(ndf[medical,"scores"])
mean(ndf[truckers,"scores"], na.rm=TRUE)
mean(ndf[military,"scores"])
mean(ndf[teachers,"scores"])
mean(ndf[under_thirty,"scores"], na.rm=TRUE)
mean(ndf[thirty_to_sixty,"scores"], na.rm=TRUE)
mean(ndf[sixty_plus,"scores"], na.rm=TRUE)

# write codings
# factor_codings = file("~/repos/signal/gss/factor_codings.txt")
# codings = NULL
# for (c in names(qs[,2])) { codings = c(codings, c, as.character(unlist(qs[,2][c])), "\n") }
# writeLines(codings, con = factor_codings)
# 
# factor_questions = file("~/repos/signal/gss/factor_questions.txt")
# questions = NULL
# for (c in names(qs[,1])) { questions = c(questions, c, as.character(unlist(qs[,1][c])), "\n") }
# writeLines(questions, con = factor_questions)


df = read.csv("~/repos/signal/gss/gss_fs.csv") #freespch

zeroes = df[df$freespch == 1,]

dim(zeroes)
sum(is.na(zeroes)) / (318 * 277)
colSums(is.na(zeroes))

table(zeroes[,"relig"])
table(zeroes[,"sex"])
table(zeroes[,"sexornt"])
table(zeroes[,"reliten"])
table(zeroes[,"class"])
table(zeroes[,"wrkgovt"])
table(zeroes[,"partyid"])
table(zeroes[,"age"])

qplot(relig, reliten, data = remove_missing(select(zeroes, relig, reliten)))

cor(df$wordsum, df$freespch, use="pairwise.complete.obs")
# [1] 0.1709037

aggregate(freespch ~ ., data=zeroes, FUN=mean)
aggregate(df, by=list(df$freespch), FUN=mean, na.rm=TRUE)
aggregate(df, by=list(df$freespch), FUN=cor, use="pairwise.complete.obs")
cm = cor(df, use="pairwise.complete.obs")
cm[is.na(cm)] = 0
corrplot(cm)

t.test(df$freespch[df$sex == 1 & ath_agnostic], df$freespch[df$sex == 2 & ath_agnostic])
cor.test(df$wordsum, df$freespch)
