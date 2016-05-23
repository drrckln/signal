examsquiz <- read.table("ExamsQuiz.txt", header=FALSE)
class(examsquiz)
head(examsquiz)
lma <- lm(examsquiz[,2] ~ examsquiz[,1])
# equivalent to
# lma <- lm(examsquiz$V2 ~ examsquiz$V1)
attributes(lma)

lmb <- lm(examsquiz[,2] ~ examsquiz[,1] + examsquiz[,3])

# don't write for-loops like this, what if 1:0?
first1 <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 1) break #break out of loop
  }
  return(i)
}
# this should use seq() instead, as a safe alternative

# Arrays and matrices are vectors, with extra class attributes

preda = function(x,k) {
  n = length(x)
  k2 = k/2
  pred = vector(length=n-k) # will contain predicted values
  for (i in 1:(n-k)) {
    if (sum(x[i:(i+(k-1))]) >= k2) {
      pred[i] = 1
    else pred[i] = 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}

predb = function(x,k) {
  n = length(x)
  k2 = k/2
  pred = vector(length=n-k)
  sm = sum(x[1:k])
  if (sm >= k2) pred[i] = 1 else pred[i] = 0
  if (n-k >= 2) {
    for (i in 2:(n-k)) {
      sm = sm + x[i+k-1] - x[i-1]
      if (sm >= k2) pred[i] = 1 else pred[i] = 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}

# or.. use cumsum(), which is cumulative sum

# RECYCLING
# FILTERING
# VECTORIZATION


