library("caret")
library("dplyr")
library("foreign")
library("dummies")
library("ggplot2")

df = read.dta("~/signal/day10/elections.dta")

df = df %>% filter((year - 1804) %% 4 == 0) %>% select(year, age:religion, vote, presvote)
lapply(df[sapply(df, is.factor)], levels) # na always appears to be factor "0.", eg factor 1

# the descriptive text looks fine to me, actually.. oh, NAs
levels(df$gender) = c("NA", "male", "female")
levels(df$race) = c("white", "black", "asian", "native american", "hispanic", "other", "NA")
levels(df$educ1) = c("NA", "grade school", "high school", "some college", "college+")
levels(df$urban) = c("NA", "central cities", "suburban", "rural, small towns")
levels(df$region) = c("NA", "northeast", "north central", "south", "west")
levels(df$income) = c("NA", "0-16th", "17-33rd", "34-67th", "68-95th", "96-100th")
levels(df$occup1) = c("NA", "professional/managerial", "clerical/sales", "skilled/semi-skilled/service", "laborers, !farm", "farm", "homemakers")
levels(df$union) = c("NA", "yes", "no")
levels(df$religion) = c("NA", "protestant", "catholic", "jewish", "other/none")
levels(df$vote) = c("NA", "no", "yes")
levels(df$presvote) = c("NA", "democrat", "republican", "3rd party")

# impute!
# but note
# that a missing survey response often carries its own information and
# corresponds to its own category, and as such should not by default be
# replaced with a NA and subsequently imputed.
# ^ this is a little worrying, though I think it's fine here as the survey
# also records "meant" NAs
ndf = data.frame(lapply(df, function(c) {
  if (is.numeric(c)) {
    c[is.na(c)] = mean(c, na.rm=TRUE)
    return(c)
  } else if (is.factor(c)) {
    c[c == "NA"] = sample(c[c != "NA" & !is.na(c)], 1, replace=TRUE) # take out recorded NAs
    c[is.na(c)] = sample(c[c != "NA" & !is.na(c)], 1, replace=TRUE) # take out actual NAs
    return(c)
  } else {
    return(c)
  }
}))

# dummies!
ndf = dummy.data.frame(ndf) #investigate model.matrix

# take a look!
mosaicplot(table(df$income, df$presvote))
mosaicplot(table(df$region, df$race))
mosaicplot(table(df$religion, df$urban))


# Predict support for George HW Bush, 1992 election, restricted to voters

bush_election = ndf %>% filter(year == 1992 & voteyes == 1) %>% select(-year, -presvotedemocrat, -`presvote3rd party`, -voteno, -voteyes, -raceother)
outcomes = bush_election %>% select(presvoterepublican)
outcomes = outcomes[,1]
features = bush_election %>% select(-presvoterepublican)
election_model = glm(presvoterepublican ~ . , bush_election, family="binomial")

param_grid = expand.grid(.alpha = 1:10 * 0.1, .lambda = seq(0, 0.25, length.out=50))
factored_outcomes = factor(outcomes, labels = c("nobush", "bush"))
control = trainControl(method="repeatedcv", repeats=1, verboseIter=TRUE, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE)
# outcomes must be factored into 2 levels, and NOT 0 or 1
# scaling all 0s can result in NaN
caret_fit = train(x=scale(features), y=factored_outcomes, method="glmnet", metric = "ROC",
                  tuneGrid=param_grid, trControl=control)
best_a = caret_fit$bestTune[[1]]
best_l = caret_fit$bestTune[[2]]
# this one wants non-factored
fitted = glmnet(x=scale(features), y=outcomes, alpha=best_a, lambda=best_l)
coef(fitted)

# Predict party support for different years, they have 1948-2002
years = unique(df$year) # so by year, by party?
party_support = ndf %>% filter(voteyes == 1) %>% select(-voteno, )
substitute
# restrict coefficients to biggest, possibly use L1 regularization


# Predict how nonvoters would have voted
nonvoters = ndf %>% filter(year == 1992 & voteno == 1) %>% select(-year, -presvotedemocrat, -presvoterepublican, -`presvote3rd party`, -voteno, -voteyes, -raceother)
predictions = predict(fitted, scale(nonvoters)) # scaling turns into matrix
predictions = data.frame(predictions)
p = ggplot(predictions, aes(x=s0)) + geom_density()
p
