library(lattice)
df = singer
m = c()
for(i in levels(df[[2]])){
  m = c(m,mean(df[df[[2]]==i,1]))
}

ptt = pairwise.t.test(df[[1]], df[[2]])


# Bass 2  Bass 1  Tenor 2 Tenor 1 Alto 2 Alto 1 Soprano 2
# Bass 1    0.9837  -       -       -       -      -      -        
# Tenor 2   0.3604  0.9837  -       -       -      -      -        
# Tenor 1   0.0103  0.0718  0.9837  -       -      -      -        
# Alto 2    5.2e-12 3.2e-11 3.6e-06 0.0014  -      -      -        
# Alto 1    < 2e-16 < 2e-16 1.1e-10 3.0e-07 0.5168 -      -        
# Soprano 2 < 2e-16 < 2e-16 1.6e-13 7.1e-10 0.0226 0.8481 -        
# Soprano 1 < 2e-16 < 2e-16 3.1e-13 1.7e-09 0.0547 0.9837 0.9837

#conforms to expectations: There is a very statistically significantly difference in height between Soprano and Bass, but not between Bass 1 and Bass 2. That make sense. 



# Movie lens:
library(readr)
library(dplyr)
ratingsDF = read.delim("ratings.dat", sep=":")
usersDF = read.delim("users.dat", sep=":")
movieslines = readLines("movies.dat")
moviesDF = data.frame(t(data.frame(sapply(movieslines, strsplit, split="::"))))


# Clean up extraneous columns
ratingsDF = ratingsDF %>% select(-X.1, -X.2, -X)
colnames(ratingsDF) = c("UserID", "MovieID", "Rating", "Timestamp")
usersDF = usersDF %>% select(-X, -X.1, -X.2, -X.3)
colnames(usersDF) = c("UserID", "Gender", "Age", "Occupation", "Zip-code")
colnames(moviesDF) = c("MovieID", "Title", "Genres")
moviesDF[[1]] = as.numeric(moviesDF[[1]])
rownames(moviesDF) = NULL

# Join
ndf = inner_join(ratingsDF, usersDF)

# For each movie, t-test the women's average vs the sample average..?

t_tests = data.frame()
colnamesDF = c("MovieID", "PValue")
for (m_id in unique(ndf$MovieID)) {
  temp_df = ndf %>% filter(MovieID == m_id)
  
  pair = unlist(c(m_id, pairwise.t.test(temp_df$Rating, temp_df$Gender)$p.value))
  t_tests = rbind(t_tests, pair)
}
colnames(t_tests) = colnamesDF
t_tests[is.na(t_tests$PValue),2] = 2
t_tests[t_tests$PValue>1,2] = NA
noNAT_tests = dplyr::filter(t_tests, !is.na(PValue))
# t_tests[t_tests$PValue<.05]
robustHigher = dplyr::filter(noNAT_tests, PValue<.05)

# OK, let's grab the movie titles
orderedRobustHigher = robustHigher[order(robustHigher$PValue),]
remaining = inner_join(robustHigher, moviesDF)
remaining$Title

orderedRemaining = inner_join(orderedRobustHigher, moviesDF)
orderedRemaining
