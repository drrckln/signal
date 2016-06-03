library("MASS")
library("klaR")
library("dplyr")
library("e1071")

# m, b are integers. label is 1 or -1
lin_pair = function(m, b, label) {
  x = runif(1)
  if (label == 1) {
    y = runif(1, min=m*x+b, max=2)
  } else {
    y = runif(1, min=-1, max=m*x+b)
  }
  return(c(x, y))
}

pts = data.frame()
for (i in seq(2000)) {
  pair = lin_pair(-1, 1, -1)
  pts = rbind(pts, pair)
}

plot(pts)
# seems to work

quad_pair = function(a, b, c, label) {
  x = runif(1)
  threshold = a*(x-b)^2 + c
  if (label == 1) {
    y = runif(1, min=threshold, max=2)
  } else {
    y = runif(1, min=-1, max=threshold)
  }
  return(c(x, y))
}


pts = data.frame()
for (i in seq(2000)) {
  pair = quad_pair(1, 0, 0, 1)
  pts = rbind(pts, pair)
}

plot(pts)

# mu vector containing mean for each dimension
# cov 2x2 covariance matrix
mvnorm_pair = function(mu, cov) {
  mvrnorm(n=1,mu=mu, Sigma=cov)
}

#generate 100 data points
# 50 from (0.25, 0.25)
# rest from (0.75, 0.75)
c_matrix = matrix(c(0.2, 0.1, 0.1, 0.2), ncol=2)

pts = data.frame()
for (i in seq(50)) {
  pt = mvnorm_pair(c(0.25, 0.25), c_matrix)
  pts = rbind(pts, pt)
}

for (i in seq(50)) {
  pt = mvnorm_pair(c(0.75, 0.75), c_matrix)
  pts = rbind(pts, pt)
}

plot(pts)

labels = c(rep(-1, 50), rep(1, 50))

lda_discr = lda(pts, labels)
qda_discr = qda(pts, labels)

# Call:
#   lda(pts, labels)
# 
# Prior probabilities of groups:
#   -1   1 
# 0.5 0.5 
# 
# Group means:
#   X0.0428068522323494 X0.621666621959587
# -1           0.2069880          0.2645083
# 1            0.7442015          0.8025272
# 
# Coefficients of linear discriminants:
#   LD1
# X0.0428068522323494 0.8312001
# X0.621666621959587  1.4218481
# 
# Call:
#   qda(pts, labels)
# 
# Prior probabilities of groups:
#   -1   1 
# 0.5 0.5 
# 
# Group means:
#   X0.0428068522323494 X0.621666621959587
# -1           0.2069880          0.2645083
# 1            0.7442015          0.8025272

lda_preds = predict(lda_discr) # accuracy 0.74
qda_preds = predict(qda_discr) # accuracy 0.71

accuracy = function(vx, vy) {
  check = unlist(Map(function(x,y) {x == y}, vx, vy))
  return(sum(check) / length(check))
}

accuracy(labels, qda_preds$class)


# Now with different covar matrices

c_matrix1 = matrix(c(0.2, 0.1, 0.1, 0.2), ncol=2)
c_matrix2 = matrix(c(0.9, 0.3, 0.3, 0.9), ncol=2)

amt = 200
pts = data.frame()
for (i in seq(amt)) {
  pt = mvnorm_pair(c(0.25, 0.25), c_matrix1)
  pts = rbind(pts, pt)
}

for (i in seq(amt)) {
  pt = mvnorm_pair(c(0.75, 0.75), c_matrix2)
  pts = rbind(pts, pt)
}

plot(pts)

labels = c(rep(-1, amt), rep(1, amt))

lda_discr = lda(pts, labels)
qda_discr = qda(pts, labels)

lda_preds = predict(lda_discr) # accuracy 0.71
qda_preds = predict(qda_discr) # accuracy 0.79, better!

accuracy(labels, qda_preds$class)

#  n  |  50|  100|   200|  400|
# LDA |0.70| 0.66| 0.700|0.735|
# QDA |0.84| 0.81| 0.765|0.810|

# uhh I can't tell.. maybe it goes up..?


# Now with linear
amt = 25
pts = data.frame()
for (i in seq(amt)) {
  pt = lin_pair(1.5, 0.2, 1)
  pts = rbind(pts, pt)
}

for (i in seq(amt)) {
  pt = lin_pair(1.5, 0.05, -1)
  pts = rbind(pts, pt)
}

plot(pts)

labels = c(rep(-1, amt), rep(1, amt))

lda_discr = lda(pts, labels)
qda_discr = qda(pts, labels)

lda_preds = predict(lda_discr) # accuracy 0.955
qda_preds = predict(qda_discr) # accuracy 0.97, better!

accuracy(labels, lda_preds$class)

#  n  |  50|  100|   200|   400|
# LDA |1.00| 0.98| 0.955|0.9650|
# QDA |0.98| 0.98| 0.970|0.9675|

# So I guess accuracy goes DOWN with n?

partimat(pts, factor(labels), method="lda")
partimat(pts, factor(labels), method="qda")
# qda has 1 error.. pretty much the same otherwise


# now let's try this on the speed dating dataset

df = read.csv("~/signal/speeddating-aggregated.csv")
ndf = df %>% select(gender, sports:yoga)
males = ndf %>% filter(gender == 1) 
females = ndf %>% filter(gender == 0)

# male means and covariance
male_means = lapply(males, mean)
male_covars = cov(males)

# female ...
female_means = lapply(females, mean)
female_covars = cov(females)


fracs = c(0.10, 0.25, 0.50, 0.75, 1)

gender_dscr = function(df, fracs, dscr) {
  accs = c()
  for (frac in fracs) {
    print(frac)
    tdf = sample_frac(df, size=frac)
    features = tdf %>% select(-gender)
    labels = tdf$gender
    preds = predict(dscr(features, labels))
    acc = accuracy(labels, preds$class)
    accs = c(accs, acc)
  }
  return(cbind(fracs, accs))
}

lda_accs = gender_dscr(ndf, fracs, lda)
qda_accs = gender_dscr(ndf, fracs, qda)

tdf = sample_frac(ndf, size=0.02)
features = tdf %>% select(-gender)
labels = tdf$gender
preds = predict(qda(features, labels))
acc = accuracy(labels, preds$class)


plot(lda_accs[,1], lda_accs[,2])
plot(qda_accs[,1], qda_accs[,2])

# next
data(wine, package="rattle")
df_wine = wine

lda_wine = lda(Type ~ ., data = df_wine)
coef(lda_wine)
#                          LD1           LD2
# Alcohol         -0.403399781  0.8717930699
# Malic            0.165254596  0.3053797325
# Ash             -0.369075256  2.3458497486
# Alcalinity       0.154797889 -0.1463807654
# Magnesium       -0.002163496 -0.0004627565
# Phenols          0.618052068 -0.0322128171
# Flavanoids      -1.661191235 -0.4919980543
# Nonflavanoids   -1.495818440 -1.6309537953
# Proanthocyanins  0.134092628 -0.3070875776
# Color            0.355055710  0.2532306865
# Hue             -0.818036073 -1.5156344987
# Dilution        -1.157559376  0.0511839665
# Proline         -0.002691206  0.0028529846

# so these are the two "lines" separating the 3 areas

wine_preds = predict(lda_wine)

ldahist(wine_preds$x[,1], df_wine$Type)
plot(wine_preds$x[,1],wine_preds$x[,2])
# ooh, so each vector is the components
# so it's comparing the weightings and you can see it causes 3 distinct groups?


# PERCEPTRONSSSS

pts = data.frame()
for (i in seq(700)) {
  pt = lin_pair(1.5, 0.2, 1)
  pts = rbind(pts, pt)
}

for (i in seq(300)) {
  pt = lin_pair(1.5, 0.05, -1)
  pts = rbind(pts, pt)
}

plot(pts)

labels = c(rep(-1, 700), rep(1, 300))

qplot(pts[[1]], pts[[2]], color=labels)

pts = cbind(pts, rep(1, nrow(pts)))

dot = function(x, y) {
  Reduce(`+`, (Map(`*`, x, y)))
}

perceptron = function(xs, y, w, rate, niter=nrow(xs)) {
  row_is = sample(seq(niter), size=niter)
  for (i in row_is) {
    cand_class = sign(dot(xs[i,], w))
    if (cand_class != y[i]) {
      if (cand_class == 1) {
        w = w - rate*xs[i,]
      } else {
        w = w + rate*xs[i,]
      }
    }
  }
  return(w)
}

set.seed(5)
colnames(pts) = c("X0", "X1", "train")
weights = perceptron(pts, labels, rep(1,3), rate=1)
weights = perceptron(pts, labels, weights, rate=1)
m = -(weights[[1]]/weights[[2]])
b = -(weights[[3]]/weights[[2]])
ggplot(pts) + geom_point(aes(x=X0, y=X1, color=labels)) + geom_abline(slope = m, intercept = b)


perceptron_conv = function(xs, y, w, rate) {
  n = 1
  classifiction = 0
  weights = perceptron(xs, y, w, rate)
  
  while (accuracy(y, classifiction) != 1) {
    n = n + 1
    
    weights = perceptron(xs, y, weights, rate)
    m = -(weights[[1]]/weights[[2]])
    b = -(weights[[3]]/weights[[2]])
    
    classifiction = unlist(Map(function(x,y) { sign(y - (m*x + b)) }, xs[,1], xs[,2]))
    # sanity check bs
    if (classifiction[1] != y[1]) {
      classifiction = -classifiction
    }
  }
  print(n)
  return(weights)
}

# variation in seeds?
# 6, 3, 4, 6, 8, 3 iterations (for seeds seq(6))
# line is jiggling up and down, slope changes a bit as well
set.seed(5)
weights = perceptron_conv(pts, labels, rep(1,3), rate=20)
m = -(weights[[1]]/weights[[2]])
b = -(weights[[3]]/weights[[2]])
ggplot(pts) + geom_point(aes(x=X0, y=X1, color=labels)) + geom_abline(slope = m, intercept = b)

# for seed(5),
# 8, 2, 5, 6, 6 .. ?
# for rate=seq(5)

# What about 20 data points?
pts = data.frame()
for (i in seq(8)) {
  pt = lin_pair(1.5, 0.2, 1)
  pts = rbind(pts, pt)
}

for (i in seq(12)) {
  pt = lin_pair(1.5, 0.05, -1)
  pts = rbind(pts, pt)
}

labels = c(rep(-1, 8), rep(1, 12))

qplot(pts[[1]], pts[[2]], color=labels)

pts = cbind(pts, rep(1, nrow(pts)))

# seed(5), rate=1, data = 20pts: took 9 iterations
# for 2000pts it took 8 iterations
# what if I change the rate, seq(5)
# 9, 12, 8, 8, 8 ... I tried rate=20 and it was still 8 iterations
# so seems to take longer than if 2000 pts


# SVM time
pts = data.frame()
amt = 1000
for (i in seq(amt)) {
  pt = lin_pair(1.5, 0.2, 1)
  pts = rbind(pts, pt)
}

for (i in seq(amt)) {
  pt = lin_pair(1.5, 0.05, -1)
  pts = rbind(pts, pt)
}

labels = c(rep(-1, amt), rep(1, amt))

qplot(pts[[1]], pts[[2]], color=labels)

pts = cbind(pts, labels)
colnames(pts) = c("X", "Y", "Class")
pts$Class = factor(pts$Class)

svm_fit = svm(Class ~ ., data=pts, cost = 0.2, kernel="linear")
plot(svm_fit, pts)
# higher cost divides all the points cleanly

# What about quadraticly separated data?
pts = data.frame()
amt = 1000
for (i in seq(amt)) {
  pt = quad_pair(3, 0.5, 0.55, 1)
  pts = rbind(pts, pt)
}

for (i in seq(amt)) {
  pt = quad_pair(3, 0.5, 0.4, -1)
  pts = rbind(pts, pt)
}

labels = c(rep(-1, amt), rep(1, amt))

qplot(pts[[1]], pts[[2]], color=labels)

pts = cbind(pts, labels)
colnames(pts) = c("X", "Y", "Class")
pts$Class = factor(pts$Class)

svm_fit = svm(Class ~ ., data=pts, kernel="linear", cost = 5)
plot(svm_fit, pts)
# well, it works crappily. Also varying C doesn't seem to change the hyperplane much


# How well does it work when each class is drawn from a diff multivariate normal distribution?
# It fails utterly
c_matrix1 = matrix(c(0.2, 0.1, 0.1, 0.2), ncol=2)
c_matrix2 = matrix(c(0.9, 0.3, 0.3, 0.9), ncol=2)

amt = 200
pts = data.frame()
for (i in seq(amt)) {
  pt = mvnorm_pair(c(0.25, 0.25), c_matrix1)
  pts = rbind(pts, pt)
}

for (i in seq(amt)) {
  pt = mvnorm_pair(c(0.75, 0.75), c_matrix1)
  pts = rbind(pts, pt)
}

plot(pts)

labels = c(rep(-1, amt), rep(1, amt))
pts = cbind(pts, labels)
colnames(pts) = c("X", "Y", "Class")
pts$Class = factor(pts$Class)

svm_fit = svm(Class ~ ., data=pts, kernel="linear", cost = 5)
plot(svm_fit, pts)

# let's try on the speed dating dataset
svm_fit = svm(gender ~ ., data=ndf, kernel="linear", cost = 5)
plot(svm_fit, pts)
# tune this to cross validate for cost C

# It seems to do the rest of this assignment I should refactor into reusable functions
