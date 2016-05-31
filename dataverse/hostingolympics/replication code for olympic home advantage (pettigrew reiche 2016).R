setwd("c:/users/stephen/dropbox/olympics/home advantage/replication files")
load("home advantage replication data.RData")

#####################
## All Olympic data collected from Sports Reference (http://www.sports-reference.com/olympics/)
#####################

#####################################################
## Get total number of athletes per county-Olympics and merge in the host country data

ath <- aggregate(rep(1,nrow(athletes)), list(session = athletes$session,
                                             year = athletes$year,
                                             country = athletes$country), sum)

names(ath)[4] <- "athletes"
h <- merge(ath, metal.count, by = c("session","year","country"), all = T)
h[is.na(h)] <- 0
h <- merge(h, hosts, by = c("session","year"), all = T)

h <- h[h$session != "equestrian",] ## drop the Equestrian Olympics in 1956
h <- h[h$year > 1945,] ## keep only the post-WWII Olympics

h$host <- h$country.x == h$country.y
h$mpa <- h$total / h$athletes
h$gpa <- h$gold / h$athletes


##################################################
## Now subset the data so that we only keep the datapoints
## from a country's host year and the previous Games

res <- list()
ii <- which(h$host & h$year > 1951)[1]
for(ii in which(h$host & h$year > 1951)){
  holder <- h[ii,]
  if(holder$year[1] == 1994){
    o <- rbind(h[h$year == (holder$year[1] - 2) & h$country.x == holder$country.x[1] & h$session == holder$session[1],],
               holder)
  }else if(holder$year[1] == 1984 & holder$session == "summer"){
    o <- rbind(h[h$year == (holder$year[1] - 8) & h$country.x == holder$country.x[1] & h$session == holder$session[1],],
               holder)
  } else{
    o <- rbind(h[h$year == (holder$year[1] - 4) & h$country.x == holder$country.x[1] & h$session == holder$session[1],],
               holder)
  }
  res[[length(res) + 1]] <- o
}

d <- list(Summer = res[sapply(res, function(x) x$session[1] == "summer")],
          Winter = res[sapply(res, function(x) x$session[1] == "winter")])




################################
## Figure 1

tiff(paste0("figure 1 - athletes.tiff"),
     width = 600, height = 600,
     res = 75)
par(mfrow = c(2,1))
ii <- 1
for(ii in 1:2){  
  data <- d[[ii]]
  holder <- fold.list(data,silent = T)
  par(mar = c(4.1, 8.5, 2, 1))
  plot(0, col = "white",
       xlim = c(0,max(holder$athletes)),
       ylim = c(1,length(data)),
       main = paste(names(d)[ii], "Olympics"),
       xlab = "Total Participating Athletes in Host Year (triangles) and Previous Games (squares)",
       ylab = "",
       yaxt = "n",
       bty = "l")
  jj <- 1
  for(jj in 1:length(data)){
    
    points(y = rep(length(data) + 1 - jj, 2),
           x = data[[jj]]$athletes,
           pch = c(15,17),
           col = c("black","red"),
           cex = 1.5)
    arrows(y0 = rep(length(data) + 1 - jj, 2),
           y1 = rep(length(data) + 1 - jj, 2),
           x0 = data[[jj]]$athletes[1],
           x1 = data[[jj]]$athletes[2],
           length = .1)
  }
  axis(2, at = length(data):1,
       sapply(data, function(x) paste0(x$country.x[1], " '", substr(x$year[2], 3,4))),
       las = 2)
}
graphics.off()




################################
## Figure 2

tiff(paste0("figure 2 - gold medals.tiff"),
     width = 600, height = 600,
     res = 75)
par(mfrow = c(2,1))
ii <- 1
for(ii in 1:2){  
  data <- d[[ii]]
  holder <- fold.list(data,silent = T)
  par(mar = c(4.1, 8.5, 2, 1))
  plot(0, col = "white",
       xlim = c(0,max(holder$gold)),
       ylim = c(1,length(data)),
       main = paste(names(d)[ii], "Olympics"),
       xlab = "Gold Medals Won in Host Year (triangles) and Previous Games (squares)",
       ylab = "",
       yaxt = "n",
       bty = "l")
  jj <- 1
  for(jj in 1:length(data)){
    if(diff(data[[jj]]$gold) != 0){
      points(y = rep(length(data) + 1 - jj, 2),
             x = data[[jj]]$gold,
             pch = c(15,17),
             col = c("black","red"),
             cex = 1.5)
      arrows(y0 = rep(length(data) + 1 - jj, 2),
             y1 = rep(length(data) + 1 - jj, 2),
             x0 = data[[jj]]$gold[1],
             x1 = data[[jj]]$gold[2],
             length = .1)
    } else{
      
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$gold[1],
             pch = c(15),
             col = c("black"),
             cex = 1.5)
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$gold[2],
             pch = c(17),
             col = c("red"),
             cex = 1.5)
    }

  }
  axis(2, at = length(data):1,
       sapply(data, function(x) paste0(x$country.x[1], " '", substr(x$year[2], 3,4))),
       las = 2)
}
graphics.off()



################################
## Figure 3

tiff(paste0("figure 3 - total medals.tiff"),
     width = 600, height = 600,
     res = 75)
par(mfrow = c(2,1))
ii <- 1
for(ii in 1:2){  
  data <- d[[ii]]
  holder <- fold.list(data,silent = T)
  par(mar = c(4.1, 8.5, 2, 1))
  plot(0, col = "white",
       xlim = c(0,max(holder$total)),
       ylim = c(1,length(data)),
       main = paste(names(d)[ii], "Olympics"),
       xlab = "Total Medals Won in Host Year (triangles) and Previous Games (squares)",
       ylab = "",
       yaxt = "n",
       bty = "l")
  jj <- 1
  for(jj in 1:length(data)){
    points(y = rep(length(data) + 1 - jj, 2),
           x = data[[jj]]$total,
           pch = c(15,17),
           col = c("black","red"),
           cex = 1.5)
    arrows(y0 = rep(length(data) + 1 - jj, 2),
           y1 = rep(length(data) + 1 - jj, 2),
           x0 = data[[jj]]$total[1],
           x1 = data[[jj]]$total[2],
           length = .075)
  }
  axis(2, at = length(data):1,
       sapply(data, function(x) paste0(x$country.x[1], " '", substr(x$year[2], 3,4))),
       las = 2)
}
graphics.off()





################################
## Figure 4

tiff(paste0("figure 4 - golds per athlete.tiff"),
     width = 600, height = 600,
     res = 75)
par(mfrow = c(2,1))
ii <- 1
for(ii in 1:2){  
  data <- d[[ii]]
  holder <- fold.list(data,silent = T)
  par(mar = c(4.1, 8.5, 2, 1))
  plot(0, col = "white",
       xlim = c(0,max(holder$gpa)),
       ylim = c(1,length(data)),
       main = paste(names(d)[ii], "Olympics"),
       xlab = "Gold Medals per Partipicant in Host Year (triangles) and Previous Games (squares)",
       ylab = "",
       yaxt = "n",
       bty = "l")
  jj <- 1
  for(jj in 1:length(data)){
    if(abs(diff(data[[jj]]$gpa)) > 0.002){
      points(y = rep(length(data) + 1 - jj, 2),
             x = data[[jj]]$gpa,
             pch = c(15,17),
             col = c("black","red"),
             cex = 1.5)
      arrows(y0 = rep(length(data) + 1 - jj, 2),
             y1 = rep(length(data) + 1 - jj, 2),
             x0 = data[[jj]]$gpa[1],
             x1 = data[[jj]]$gpa[2],
             length = .1)
    }else{
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$gpa[1],
             pch = c(15),
             col = c("black"),
             cex = 1.5)
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$gpa[2],
             pch = c(17),
             col = c("red"),
             cex = 1.5)
    }
  }
  axis(2, at = length(data):1,
       sapply(data, function(x) paste0(x$country.x[1], " '", substr(x$year[2], 3,4))),
       las = 2)
}
graphics.off()





################################
## Figure 5

tiff(paste0("figure 5 - medals per athlete.tiff"),
     width = 600, height = 600,
     res = 75)
par(mfrow = c(2,1))
ii <- 1
for(ii in 1:2){  
  data <- d[[ii]]
  holder <- fold.list(data,silent = T)
  par(mar = c(4.1, 8.5, 2, 1))
  plot(0, col = "white",
       xlim = c(0,max(holder$mpa)),
       ylim = c(1,length(data)),
       main = paste(names(d)[ii], "Olympics"),
       xlab = "Total Medals per Partipicant in Host Year (triangles) and Previous Games (squares)",
       ylab = "",
       yaxt = "n",
       bty = "l")
  jj <- 1
  for(jj in 1:length(data)){
    if(abs(diff(data[[jj]]$mpa)) > 0.01){
      points(y = rep(length(data) + 1 - jj, 2),
             x = data[[jj]]$mpa,
             pch = c(15,17),
             col = c("black","red"),
             cex = 1.5)
      arrows(y0 = rep(length(data) + 1 - jj, 2),
             y1 = rep(length(data) + 1 - jj, 2),
             x0 = data[[jj]]$mpa[1],
             x1 = data[[jj]]$mpa[2],
             length = .1)
    } else if(abs(diff(data[[jj]]$mpa)) > 0.0002){
      points(y = rep(length(data) + 1 - jj, 2),
             x = data[[jj]]$mpa,
             pch = c(15,17),
             col = c("black","red"),
             cex = 1.5)
    } else{
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$mpa[1],
             pch = c(15),
             col = c("black"),
             cex = 1.5)
      points(y = length(data) + 1 - jj,
             x = data[[jj]]$mpa[2],
             pch = c(17),
             col = c("red"),
             cex = 1.5)
    }
  }
  axis(2, at = length(data):1,
       sapply(data, function(x) paste0(x$country.x[1], " '", substr(x$year[2], 3,4))),
       las = 2)
}
graphics.off()






############################
## Reorganize the dataset to run some regressions

r <- fold.list(list(fold.list(d[[1]]), fold.list(d[[2]])))
r$country.y <- NULL
r <- plyr::rename(r, c("country.x" = "country"))
r$city[!r$host] <- ""
r$unique <- factor(paste(r$session, r$year))




######################################
## Regression analyses

## Check for host advantage in total athletes participating

m <- lm(athletes ~ unique + host,
        data = r[r$session == "winter",])
summary(m) ## extra 28.1 in winter, p=0.007
mean(coef(m)[length(m)] / r$athletes[seq(33,66,2)]) # percent increase


m <- lm(athletes ~ unique + host-1,
        data = r[r$session == "summer",])
summary(m) ## extra 162.2 gold medals in summer, p=0.001
mean(coef(m)[length(m)] / r$athletes[seq(1,32,2)]) # percent increase





## Check for host advantage in total gold medals

summary(lm(gold ~ unique + host,
           data = r)) ## On average they win 4.4 extra gold medals, but not statistically signif
summary(lm(gold ~ unique + host,
           data = r[r$session == "winter",])) ## extra 1.94 gold medals  in winter, but not statistically signif
summary(lm(gold ~ unique + host,
           data = r[r$session == "summer",])) ## extra 7.2 gold medals in summer, but not statistically signif


## Check for host advantage in total medals

summary(lm(total ~ unique + host,
           data = r)) ## On average they win 7.4 extra medals, but not statistically signif
summary(lm(total ~ unique + host,
           data = r[r$session == "winter",])) ## extra 3.8 in winter, but not signif.
summary(lm(total ~ unique + host,
           data = r[r$session == "summer",])) ## extra 11.6 in summer, but not signif.


## Check for host advantage in gold medals per athlete

summary(lm(gpa ~ unique + host,
           data = r)) ## On average they have 0.003 more golds per athlete, but not statistically signif
summary(lm(gpa ~ unique + host,
           data = r[r$session == "winter",])) ## extra 0.006 more in winter, but not statistically signif
summary(lm(gpa ~ unique + host,
           data = r[r$session == "summer",])) ## extra 0.00008 fewer medals in summer, but not statistically signif


## Check for host advantage in total medals per athlete

summary(lm(mpa ~ unique + host,
           data = r)) ## On average they have 0.01 fewer medals per athlete, but not statistically signif
summary(lm(mpa ~ unique + host,
           data = r[r$session == "winter",])) ## 0.001 more in winter, but not statistically signif
summary(lm(mpa ~ unique + host,
           data = r[r$session == "summer",])) ## 0.02 fewer in summer, but not statistically signif
