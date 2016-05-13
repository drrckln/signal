library(ggplot2)
library(dplyr)
library(pROC)
df = read.csv("~/signal/day9/speeddating-aggregated.csv")

# gender ~ activities + career

ldf = select(df, gender, career_c, sports:yoga)
ldf$gender = factor(ldf$gender, levels = 0:1)
ldf$career_c = factor(ldf$career_c, levels = 1:17)

m = glm(gender ~ ., ldf, family = "binomial")
summary(m)
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    3.51477    1.03966   3.381 0.000723 ***
# career_c2     -0.69972    0.44735  -1.564 0.117784    
# career_c3     -3.01870    1.14020  -2.648 0.008109 ** 
# career_c4     -0.45419    0.62258  -0.730 0.465683    
# career_c5     -0.84472    0.75759  -1.115 0.264843    
# career_c6      0.05279    0.52873   0.100 0.920470    
# career_c7      0.39046    0.44660   0.874 0.381961    
# career_c8     15.64112 2215.13070   0.007 0.994366    
# career_c9     -1.63757    0.61945  -2.644 0.008203 ** 
# career_c10    -1.58319    0.63191  -2.505 0.012231 *  
# career_c11   -18.88968  887.28658  -0.021 0.983015    
# career_c12   -18.37372 2771.42980  -0.007 0.994710    
# career_c13     0.11939    1.09155   0.109 0.912902    
# career_c14    13.82415 3956.18038   0.003 0.997212    
# career_c15     1.29973    1.31412   0.989 0.322639    
# career_c16   -17.96894 2605.37402  -0.007 0.994497    
# career_c17    15.77156 3956.18038   0.004 0.996819    
# sports         0.22133    0.06078   3.642 0.000271 ***
# tvsports       0.02745    0.05107   0.537 0.590943    
# exercise      -0.10469    0.05675  -1.845 0.065073 .  
# dining        -0.07146    0.07842  -0.911 0.362179    
# museums        0.03763    0.12049   0.312 0.754800    
# art           -0.06380    0.10301  -0.619 0.535718    
# hiking        -0.17102    0.05278  -3.240 0.001196 ** 
# gaming         0.32317    0.05272   6.130 8.76e-10 ***
# clubbing      -0.13516    0.05084  -2.658 0.007852 ** 
# reading       -0.04805    0.06181  -0.777 0.436900    
# tv            -0.14675    0.05877  -2.497 0.012515 *  
# theater       -0.16621    0.07184  -2.314 0.020684 *  
# movies         0.16245    0.08579   1.893 0.058295 .  
# concerts      -0.01566    0.07849  -0.199 0.841877    
# music          0.02886    0.08633   0.334 0.738101    
# shopping      -0.26254    0.05822  -4.510 6.49e-06 ***
# yoga          -0.06997    0.04620  -1.515 0.129854    

p = ggplot(ldf) + geom_jitter(aes(gender, career_c))
p

# gender in terms of activities
m = glm(gender ~ .-career_c, ldf, family = "binomial")
summary(m)
 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.694138   0.873389   3.085 0.002038 ** 
# sports       0.229420   0.056073   4.091 4.29e-05 ***
# tvsports     0.018588   0.046809   0.397 0.691292    
# exercise    -0.108370   0.051813  -2.092 0.036480 *  
# dining      -0.004022   0.072516  -0.055 0.955771    
# museums      0.058433   0.111207   0.525 0.599276    
# art         -0.063445   0.094167  -0.674 0.500469    
# hiking      -0.178810   0.049036  -3.646 0.000266 ***
# gaming       0.290660   0.047363   6.137 8.42e-10 ***
# clubbing    -0.089037   0.045516  -1.956 0.050444 .  
# reading     -0.049494   0.058036  -0.853 0.393760    
# tv          -0.105014   0.053147  -1.976 0.048164 *  
# theater     -0.187841   0.065050  -2.888 0.003881 ** 
# movies       0.130086   0.078836   1.650 0.098925 .  
# concerts    -0.016680   0.073047  -0.228 0.819377    
# music        0.019306   0.079888   0.242 0.809042    
# shopping    -0.275804   0.053105  -5.194 2.06e-07 ***
# yoga        -0.082007   0.042350  -1.936 0.052815 .  
probs = fitted(m)
actual = ldf$gender_o
r = roc(actual,probs)
plot(r)

# restrict to career code 2 or 7

ldf = select(df, gender, career_c, sports:yoga)
ldf = filter(ldf, career_c == 2 | career_c == 7)
ldf$gender = factor(ldf$gender, levels = 0:1)
ldf$career_c = factor(ldf$career_c, levels = c(2, 7))

m = glm(career_c ~ ., ldf, family = "binomial")
summary(m)
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -1.573637   1.238707  -1.270  0.20395   
# gender1      1.088953   0.331633   3.284  0.00102 **
# sports       0.074634   0.067946   1.098  0.27202   
# tvsports     0.038049   0.059138   0.643  0.51997   
# exercise     0.049050   0.063965   0.767  0.44319   
# dining       0.198741   0.087758   2.265  0.02353 * 
# museums     -0.184647   0.135819  -1.360  0.17399   
# art          0.003090   0.116435   0.027  0.97882   
# hiking      -0.085774   0.059457  -1.443  0.14913   
# gaming       0.085247   0.059535   1.432  0.15218   
# clubbing     0.150621   0.062881   2.395  0.01661 * 
# reading     -0.068683   0.077235  -0.889  0.37385   
# tv           0.082282   0.065362   1.259  0.20808   
# theater      0.049805   0.085982   0.579  0.56242   
# movies      -0.052658   0.094302  -0.558  0.57657   
# concerts    -0.136038   0.089961  -1.512  0.13048   
# music       -0.001155   0.094045  -0.012  0.99020   
# shopping    -0.044004   0.065871  -0.668  0.50411   
# yoga         0.073289   0.054672   1.341  0.18008   
probs = fitted(m)
actual = ldf$career_c
r = roc(actual,probs)
plot(r)


# restrict to race 2 or 4
ldf = select(df, gender, race, sports:yoga)
ldf = filter(ldf, race == 2 | race == 4)
ldf$gender = factor(ldf$gender, levels = 0:1)
ldf$career_c = factor(ldf$career_c, levels = 1:17)
ldf$race = factor(ldf$race, levels = c(2, 4))

m = glm(race ~ ., ldf, family = "binomial")
summary(m)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -0.83973    0.92578  -0.907  0.36438   
# gender1     -0.06443    0.26576  -0.242  0.80843   
# sports      -0.04496    0.05656  -0.795  0.42661   
# tvsports     0.11092    0.05044   2.199  0.02789 * 
# exercise    -0.13524    0.05356  -2.525  0.01157 * 
# dining       0.05093    0.07367   0.691  0.48934   
# museums     -0.13458    0.11741  -1.146  0.25171   
# art          0.02675    0.10670   0.251  0.80206   
# hiking       0.03581    0.04898   0.731  0.46465   
# gaming       0.05187    0.04735   1.095  0.27340   
# clubbing    -0.01396    0.04804  -0.291  0.77135   
# reading     -0.02288    0.06037  -0.379  0.70473   
# tv           0.10504    0.05809   1.808  0.07055 . 
# theater      0.02319    0.07458   0.311  0.75586   
# movies      -0.06992    0.08132  -0.860  0.38987   
# concerts     0.03897    0.07674   0.508  0.61153   
# music       -0.06152    0.08379  -0.734  0.46283   
# shopping     0.16736    0.05853   2.859  0.00425 **
# yoga        -0.01703    0.04597  -0.370  0.71109   

probs = fitted(m)
actual = ldf$race
r = roc(actual,probs)
plot(r)
