### clustering for LW dataset

# load in
library(readr)
library(cluster)
library(pvclust)
library(ggplot2)
library(fpc)
library(datasets)
library(mixtools)
library(mclust)
library(psych)
library(dplyr)

setwd("C:/Users/Dave/Documents/Signal")

df = read.csv('cleaned_LW_data.csv')
original_df = read.csv('2016_lw_survey_public_release_3.csv')

# Pick out the numeric features
# Exclude Age, Income, IncomeCharityPortion and XriskCharity

num_df = df %>% select(-X, -Age, -Income, -IncomeCharityPortion, -XriskCharity)
str(original_df)

selector = sapply(original_df, is.factor)
factored = colnames(original_df)[selector]

#check if col is in selector

num_df = num_df %>% select(-one_of(factored))

# compute the correlation matrix
cmatrix = cor(num_df, use="pairwise.complete.obs")
eucDist = dist(num_df,method = 'euclidean')
clust = hclust(eucDist, method = 'ward.D2')

hclust_plot = function(eucDist, method, k, df) {
  cut = hclust(eucDist,method = method)
  tree = cutree(cut,k)
  clusplot(df, tree)
}

for (k in 2:5){
  hclust_plot(eucDist,'ward.D2', k, num_df)
}


### kmeans on lw

kmeansLW = kmeans(num_df,10)
clusplot(num_df, kmeansLW$cluster, color = T, shade = T, labels = 2, lines = 0)

kmeans_plot = function(data, k){
  toplot = kmeans(data, k)
  #clusplot(toplot$cluster, )
  clusplot(data, toplot$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
}

kmeans_plot(num_df,5)


