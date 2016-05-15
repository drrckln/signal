library("caret")
library("dplyr")
library("tm")
library("readr")
library("SnowballC")
install.packages("SnowballC")

corpus = read_csv("~/signal/day10/spam-emails.csv")
vs = VectorSource(corpus$Message)
vc= VCorpus(vs)
vc <- tm_map(vc, content_transformer(tolower), lazy= TRUE)
vc <- tm_map(vc, content_transformer(removePunctuation), lazy = TRUE)
vc <- tm_map(vc, content_transformer(removeNumbers), lazy = TRUE)
vc <- tm_map(vc, removeWords, stopwords("english"), lazy = TRUE)

dtm = DocumentTermMatrix(vc)

# pick only terms that occur 10x+
dtm = dtm[,findFreqTerms(dtm, lowfreq=10)]


# find out which emails are spam, through inner join (put in right order)
lines = readLines("~/signal/day10/spam-emails-key.txt")
labs = sapply(1:length(lines), function(i){strsplit(lines[i], " ")[[1]][1]})
names = sapply(1:length(lines), function(i){strsplit(lines[i], " ")[[1]][2]})
kdf = data.frame(names, as.numeric(labs))
kdf$names = as.character(kdf$names)
colnames(kdf)[1] = colnames(corpus)[[1]]

joined = inner_join(corpus, kdf)
names(joined)[7] = "label"

# can I pass the matrix in?

outcomes = joined$label
param_grid = expand.grid(.alpha = 1:10 * 0.1, .lambda = seq(0, 0.25, length.out=50))
factored_outcomes = factor(joined$label, labels = c("notspam", "spam"))
control = trainControl(method="repeatedcv", repeats=1, verboseIter=TRUE, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE)
# outcomes must be factored into 2 levels, and NOT 0 or 1
# scaling all 0s can result in NaN
caret_fit = train(x=dtm, y=factored_outcomes, method="glmnet", metric = "ROC",
                  tuneGrid=param_grid, trControl=control)
best_a = caret_fit$bestTune[[1]]
best_l = caret_fit$bestTune[[2]]
# this one wants non-factored
fitted = glmnet(x=dtm, y=outcomes, alpha=best_a, lambda=best_l)