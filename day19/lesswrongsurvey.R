library(glmnet)
library(dplyr)
setwd("C:/Users/Dave/Documents/Signal/")

df = read.csv('2016_lw_survey_public_release_3.csv')

df = df[df$ResearchConsent == 'Yes',]
df = df[!is.na(df$Age),]
df = df[df$Age < 90,]
df = df[df$Age > 0,]
df = df[df$IQ < 200,]
df = df[df$IQ > 0,]
df = df[df$SAT <= 1600,]
df = df[df$SAT2 <= 2400,]
df = df[df$Depression != "",]

dropwriteins = function(df){
  colstosave = c()
  
  stringstodrop = c('other')
  
  for (i in 1:ncol(df)){
    curcol = df[[i]]
    curcolname = names(df)[i]
    
    toadd = c(TRUE)
    
    # remove keywords
    if(length(grep("other", curcolname)) > 0)        toadd = c(toadd,FALSE)
    if(length(grep("comment", curcolname)) > 0)        toadd = c(toadd,FALSE)
    if(length(grep("Probability", curcolname)) > 0)  toadd = c(toadd,FALSE)
    if(length(grep("CharityDonations", curcolname)) > 0)      toadd = c(toadd,FALSE)
    if(length(grep("Peak", curcolname)) > 0)         toadd = c(toadd,FALSE)
    if(length(grep("Calibration", curcolname)) > 0)  toadd = c(toadd,FALSE)
    if(length(grep("write", curcolname)) > 0)        toadd = c(toadd,FALSE)
    if(length(grep("Write", curcolname)) > 0)        toadd = c(toadd,FALSE)
    if(length(grep("biggest problems", curcolname)) > 0)      toadd = c(toadd,FALSE)
    if(length(grep("biggest problems", curcolname)) > 0)      toadd = c(toadd,FALSE)
    
    
    #check unique/NA ratio and filter
    numuniques = table(curcol)
    numnas = sum(is.na(curcol))
    naratio = length(numuniques) / numnas
    if (length(levels(curcol)) > 7){
      if (naratio > 1) toadd = c(toadd,FALSE)
    }  
    
    # drop cols with single unique value
    if (length(unique(curcol)) <= 2) toadd = c(toadd,FALSE)
    
    # add column to saving mask
    if (all(toadd)) colstosave = c(colstosave,i)
    
    
    ### modify factors
    if (class(curcol) == 'factor'){
      curlevels = levels(curcol)
      curcol = factor(curcol,levels = c(levels(curcol)))
      addNA(curcol)
    }
  }
  return(select(df,colstosave))
}

newdf = dropwriteins(df)
names(newdf)
