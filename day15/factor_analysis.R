library(psych)
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(glmnet)
setwd("~/repos/signal/")

# Convenience function for calculating proportion of NAs
prop_na = function(c, t=0.98) sum(is.na(c)) / length(c) > t

# Coerce factors to numerics
coerce = function(df) {
  for (i in seq(df)) {
    df[[i]] = as.numeric(df[[i]]) - 1
  }
  return(df)
}

# Remove non-binary columns
# this cuts from 2620 questions to 1100.. damn
binary_filter = function(df) {
  for (n in names(df)) {
    if (length(unique(df[[n]])) > 3) {
      # Filter for questions with binary responses
      df[[n]] = NULL
    }
  }
  return(df)
}

# Remove proportion of NAs
cols_mostly_NA = function(df, t=0.98) {
  names_rm = names(df)[sapply(df, prop_na, t=t)]
}




# Main
df = readRDS('parsed_data.rds')
df = select(df, -starts_with("p_")) # remove OKC personality vars
set.seed(1)
load("factors")

# Load question text
qdata = read_delim('question_data.csv', ';')
names(qdata)[1] = "id"

# Make dataframe with just questions
df_qs = df %>% select(starts_with("q"))
df_qs = binary_filter(coerce(df_qs))

all = df_qs %>% filter(q76 == 1 | q76 == 0)
larks = df_qs %>% filter(q76 == 1)
owls = df_qs %>% filter(q76 == 0)

# Retain the target var!
all_q76 = all %>% select(q76)
larks_q76 = larks %>% select(q76)
owls_q76 = owls %>% select(q76)

# Find the columns that have too many NAs
names_rm = union(union(cols_mostly_NA(larks), cols_mostly_NA(owls)), cols_mostly_NA(all))

# Remove q76 and names_rm from features
all = all %>% select(-q76, -one_of(names_rm))
larks = larks %>% select(-q76, -one_of(names_rm))
owls = owls %>% select(-q76, -one_of(names_rm))

# Compute the correlations, replace NAs with 0
c_all = cor(all, use="pairwise.complete.obs")
c_larks = cor(larks, use="pairwise.complete.obs")
c_owls = cor(owls, use="pairwise.complete.obs")
c_all[is.na(c_all)] = 0
c_larks[is.na(c_larks)] = 0
c_owls[is.na(c_owls)] = 0
save(c_all, c_larks, c_owls, file="factors")

# Eigenvalues, plotted (scree test)
eig_all = eigen(c_all)
eig_larks = eigen(c_larks)
eig_owls = eigen(c_owls)
qplot(1:80, eig_all$values[order(abs(eig_all$values), decreasing=TRUE)][1:80]) # so, 16 or 18
qplot(1:80, eig_larks$values[order(abs(eig_larks$values), decreasing=TRUE)][1:80]) # maybe 16
qplot(1:80, eig_owls$values[order(abs(eig_owls$values), decreasing=TRUE)][1:80]) # 10

# Factor analysis on correlation matrix
n_fact_all = 18
n_fact_larks = 16
n_fact_owls = 11
fa_cor_all = fa(c_all, n_fact_all, rotate="oblimin", fm="pa", covar=TRUE)
fa_cor_larks = fa(c_larks, n_fact_larks, rotate="oblimin", fm="pa", covar=TRUE)
fa_cor_owls = fa(c_owls, n_fact_owls, rotate="oblimin", fm="pa", covar=TRUE)

# Turn loadings into dataframes
load_all = as.data.frame(fa_cor_all$loadings[, seq(n_fact_all)])
load_larks = as.data.frame(fa_cor_larks$loadings[, seq(n_fact_larks)])
load_owls = as.data.frame(fa_cor_owls$loadings[, seq(n_fact_owls)])

# Look at questions associated with each factor
n_qs = 10
fact_all = lapply(load_all, function(c) qdata$text[order(abs(c), decreasing=TRUE)[1:n_qs]])
fact_larks = lapply(load_larks, function(c) qdata$text[order(abs(c), decreasing=TRUE)[1:n_qs]])
fact_owls = lapply(load_owls, function(c) qdata$text[order(abs(c), decreasing=TRUE)[1:n_qs]])

# matrix multiply so you have features, in the new basis (factors)
# now you can make predictions
all[is.na(all)] = 0
features = data.matrix(all) %*% data.matrix(load_all)

# need to impute, probably
cvfit = cv.glmnet(features, all_q76[[1]], family = "binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
fit = glmnet(features, all_q76[[1]], family = "binomial")


#                        1
# (Intercept)  0.435736007
# PA1          0.052610915
# PA16         0.012379111
# PA6          0.001081535
# PA14         0.018301472
# PA5         -0.005480588
# PA8         -0.117607654
# PA2         -0.047489362
# PA18        -0.142879556
# PA13         0.109474533
# PA3         -0.003722863
# PA9          0.019382425
# PA4         -0.034114169
# PA17         0.035272036
# PA10        -0.042522702
# PA12        -0.101207005
# PA15         0.010174854
# PA7          0.020783351
# PA11         0.015675309



# Define function to get top 10 loadings given load_* and col #
top_load = function(loadings, c) loadings[order(abs(loadings[[c]]), decreasing=TRUE), c][1:10]

# Fix signs of factors
switch_male = c()
switch_female = c()
load_male[, switch_male] = -load_male[, switch_male]
load_female[, switch_female] = -load_female[, switch_female]

# Set factor names for columns
fact_names_male = c("sex_driven", "Christian", "spiritual",
                    "artistic", "hateful", "independent",
                    "tolerates_problems", "transactional",
                    "extroverted", "kinky", "technical",
                    "nerdy", "govt_ok", "celibate_ok",
                    "pacifist", "vulgar",
                    "other1", "other2")
fact_names_female = c("sex_driven", "sophisticated",
                      "transactional", "spiritual", "celibate_ok",
                      "tolerates_problems", "Christian",
                      "pacifist", "other")
colnames(load_male) = fact_names_male
colnames(load_female) = fact_names_female

# Fill in the data with column means
imputed_all = df_qs_all
imputed_m = df_qs_m
imputed_f = df_qs_f
for (n in names(imputed_m)) {
  #imputed_all[[n]][is.na(imputed_all[[n]])] = mean(imputed_all[[n]], na.rm=TRUE)
  imputed_m[[n]][is.na(imputed_m[[n]])] = mean(imputed_m[[n]], na.rm=TRUE)
  #imputed_f[[n]][is.na(imputed_f[[n]])] = mean(imputed_f[[n]], na.rm=TRUE)
}

c_scores_m = cor(scores_m)
corrplot(c_scores_m)

# Calculate factor scores for imputed
scores_m = as.matrix(imputed_m) %*% as.matrix(load_male)[, 1:16]
colnames(scores_m) = fact_names_male[1:16]
fa_imp_m = fa(cor(scores_m), 5, rotate="oblimin", fm="pa", covar=TRUE)
corrplot(fa_imp_m$loadings, is.corr=FALSE, cl.pos="n")

scores_m_scale = scale(scores_m)
df_m = df[df$gender == "Man", ]
aggregate(-scores_m_scale, list(df_m$d_drugs), mean)