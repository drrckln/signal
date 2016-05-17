library("dplyr")
library("ggplot2")

df = read.csv("~/signal/speeddating-aggregated.csv")
df = df[-1]
str(df)


p = prcomp(df, scale=TRUE)
summary(p)
p$rotation
p$x
