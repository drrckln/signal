library(psych)
library(dplyr)
library(ggplot2)
library(readr)
setwd('~/signal')
df = readRDS('parsed_data.rds')

set.seed(1)

# Load question text
qdata = read_delim('question_data.csv', ';')
names(qdata)[1] = "id"

# Make dataframe with just questions
df_qs = select(df, starts_with("q"))

# Remove particular columns
for (n in names(df_qs)) {
  if (length(unique(df_qs[[n]])) > 3) {
    # Filter for questions with binary responses
    df_qs[[n]] = NULL
  } else if (sum(is.na(df_qs[[n]])) / length(df_qs[[n]]) > 0.98) {
    # Filter for questions with proportion of NAs > 0.98
    df_qs[[n]] = NULL
  }
}

# Coerce factors to numerics
for (i in 1:length(df_qs)) {
  df_qs[[i]] = as.numeric(df_qs[[i]]) - 1
}

# Generate data frames for Male/Female
df_qs_all = df_qs[df$gender %in% c("Man", "Woman"), ]
df_qs_m = df_qs[df$gender == "Man", ]
df_qs_f = df_qs[df$gender == "Woman", ]

# Filter each one for proportion of NAs in column > 0.98
prop_na = function(c, t=0.98) sum(is.na(c)) / length(c) > t
sd_zero = function(c) sd(c, na.rm=TRUE) < 0.01
names_rm = union(names(df_qs_m)[sapply(df_qs_m, prop_na)], names(df_qs_f)[sapply(df_qs_f, prop_na)])
df_qs_all = select(df_qs_all, -one_of(names_rm))
df_qs_m = select(df_qs_m, -one_of(names_rm))
df_qs_f = select(df_qs_f, -one_of(names_rm))

# Filter for specific genders
c_all = cor(df_qs_all, use="pairwise.complete.obs")
c_male = cor(df_qs_m, use="pairwise.complete.obs")
c_female = cor(df_qs_f, use="pairwise.complete.obs")

# Look at which female correlations are NA
tmp_1 = (1:ncol(c_female))[sapply(as.data.frame(c_female), function(c) sum(is.na(c)) > 0)]
tmp_2 = sapply(tmp_1, function(c) (1:nrow(c_female))[is.na(c_female[, c])])

# Replace them all with 0
c_female[is.na(c_female)] = 0

# Eigenvalues
eig_all = eigen(c_all)
eig_male = eigen(c_male)
eig_female = eigen(c_female)

# Plot eigenvalues (scree plot)
qplot(1:80, eig_all$values[order(abs(eig_all$values), decreasing=TRUE)][1:80])
qplot(1:80, eig_male$values[order(abs(eig_male$values), decreasing=TRUE)][1:80])
qplot(1:80, eig_female$values[order(abs(eig_female$values), decreasing=TRUE)][1:80])

# Factor analysis on correlation matrix
n_fact_all = 16
n_fact_male = 18
n_fact_female = 9
fa_cor_all = fa(c_all, n_fact_all, rotate="oblimin", fm="pa", covar=TRUE)
fa_cor_male = fa(c_male, n_fact_male, rotate="oblimin", fm="pa", covar=TRUE)
fa_cor_female = fa(c_female, n_fact_female, rotate="oblimin", fm="pa", covar=TRUE)

# Turn loadings into dataframes
load_all = as.data.frame(fa_cor_all$loadings[, 1:n_fact_all])
load_male = as.data.frame(fa_cor_male$loadings[, 1:n_fact_male])
load_female = as.data.frame(fa_cor_female$loadings[, 1:n_fact_female])

# Look at questions associated with each factor
n_qs = 10
fact_all = lapply(load_all, function(c) qdata$text[order(abs(c), decreasing=TRUE)[1:n_qs]])
fact_male = lapply(load_male, function(c) qdata$text[match(rownames(load_male)[order(abs(c), decreasing=TRUE)[1:n_qs]], qdata$id)])
fact_female = lapply(load_female, function(c) qdata$text[order(abs(c), decreasing=TRUE)[1:n_qs]])

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

