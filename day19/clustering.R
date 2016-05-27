### Clustering
library(readr)
library(cluster)
library(pvclust)
library(ggplot2)
library(fpc)
library(datasets)
library(mixtools)
library(mclust)
#load in protein

setwd("C:/Users/Dave/Documents/Signal")

df = read_delim('protein.txt','\t')
colnames(df) = c("Country", "RedMeat", "WhiteMeat", "Eggs", "Milk", 
                 "Fish", "Cereals", "Starch", "Nuts", "Fr&Veg")

countries = df[[1]]
ndf = df[,2:ncol(df)]
ndf = scale(ndf)

rownames(ndf) = countries

### Hierarchical Clustering ######

distance = dist(df,method = 'euclidean')

clust = hclust(distance, method = 'ward.D2')
plot(clust)

# plot(cutree(clust, k=4))

print_clusters = function(labels, k) {
  for (i in seq(k)) {
    print(paste("cluster", i))
    print(df[labels == i, c("Country", "RedMeat",
                                  "Fish", "Fr&Veg")])
  }
}

# Do this for k: 2-5
print_clusters(cutree(clust, k=5), 5)

# deprecated plot function
#hclust_plot = function(d, method, k) {
#  cut = 
#  clusplot(d, cut, color=TRUE, shade=TRUE, labels=2, lines=0)
#}

#distance = dist(ndf,method = 'euclidean')
#for (k in seq(2, 5, 1))
#  hclust_plot(distance, "ward.D2", k)


# dave's rework
hclust_plot = function(d, method, k) {
  cut = hclust(d,method = method)
  tree = cutree(cut,k)
  clusplot(ndf, tree)
}

for (k in 2:5){
  hclust_plot(distance,'ward.D2', k)
}


### Validating Clusters #####
#use ndf since it's scaled

pvc = pvclust(t(ndf),method.hclust = 'ward.D2', method.dist = 'euclidean')
pvrect(pvc, alpha=0.9)

qplot(1, 2)

#### K-means clustering ###############

kproteins = kmeans(ndf,5)

kmeans_plot = function(data, k){
  toplot = kmeans(data, k)
  #clusplot(toplot$cluster, )
  clusplot(data, toplot$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
}


toreplace = ndf[1,]*3
shittydf = rbind(ndf,toreplace)
kmeans_plot(shittydf,5)

kmr_ch = kmeansruns(ndf, krange=seq(10), criterion="ch")
kmr_asw = kmeansruns(ndf, krange=seq(10), criterion="asw")
kmr_ch$crit
plot(kmr_ch$crit) # 2?
plot(kmr_asw$crit) # 3?

cboot = clusterboot(ndf, clustermethod = kmeansCBI, runs=100, iter.max=100, krange=5)
# > cboot$bootmean
# [1] 0.7458016 0.7348333 0.6692976 0.8267143 0.8415000
# k = 5 is best!
# > cboot$bootbrd
# [1] 17 36 42 24 14
# So cluster 3 dissolved 42 times, that is: {Czech, Hungary, E Germany, USSR, Poland}



### Univariate models ####################


df_faithful = faithful
hist(df_faithful$waiting)
hist(df_faithful$eruptions)

toplot = normalmixEM(df_faithful$waiting)
plot(toplot,density = TRUE)

toplot = normalmixEM(df_faithful$waiting, k = 3)
summary(toplot)
plot(toplot,density = TRUE,which = 2)

# section on semiparametric

spara = spEMsymloc(c(df_faithful$waiting, 300), mu0 = 2, bw = 2)
plot(spara)


### Multivariate models ####################

plot(df_faithful$waiting, df_faithful$eruptions)
# looks like 2 clusters

plot(Mclust(scale(df_faithful)))

plot(Mclust(ndf))
# well it's confusing because it's higher dimensional..
# seems... symmetric?


### Black box models ####################

nonpara = npEM(ndf,mu0 = 3)
plot(nonpara)


### ANdrew's nonlinear bit ##############

install.packages('Rtsne')
library('Rtsne')
set.seed(1)
t = Rtsne(ndf, dims=2, perplexity=5)
plot(t$Y)
text(t$Y, labels=rownames(ndf))
