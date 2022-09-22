library(ISLR)
library("tibble")
library(splines)
attach(Wage)
wage_data <- as_tibble(Wage)

gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

install.packages("gam")
library(gam)
gam2 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(1,3))
 
plot.Gam(gam1, se=TRUE, col="blue") # because gam1 has not been created using gam library
plot(gam2, se=TRUE, col="red")

gam.m1 <- gam(wage~ns(age,5)+education,data=Wage)
gam.m2 <- gam(wage~poly(year,1)+ns(age,5)+education,data=Wage)
gam.m3 <- gam(wage~s(year,4)+ns(age,5)+education,data=Wage)
anova(gam.m1, gam.m2, gam.m3)


# GAM for classification problem
gam.logreg <- gam(I(wage>250)~ year + s(age,df=5) + education,
                  family=binomial, 
                  data=Wage)
par(mfrow=c(1,3))
plot(gam.logreg, se=T, col="blue")


# inspecting high earners data
wage_data <- as_tibble(Wage)

par(mfrow=c(1,1))
high_earners_data <- wage_data[wage_data$wage > 200,]
plot(high_earners_data$education)
sum(high_earners_data$education == "1. < HS Grad")
high_earners_data$education == "1. < HS Grad"
sum(high_earners_data$education == "1. < HS Grad")


high_earners <- wage_data[which(wage_data$wage > 200),]


'''
# remove out liers (educatoin related outliers)
gam.logreg_1 <- gam(I(wage>250)~ education,
                  family=binomial, 
                  data=Wage)

stu_residuals_1 <- rstudent(gam.logreg_1)
plot(stu_residuals_1, type ="h")
outliers_index_1 <- which(a(stu_residuals_1)>3)
new_wage_data_1 <- wage_data[-outliers_index_1,]

# retrain my GAM (educatoin related outliers)
new_gam.logreg_1 <- gam(I(wage>250)~ year + s(age,df=5) + education,
                  family=binomial, 
                  data=new_wage_data_1)
par(mfrow=c(1,3))
plot(new_gam.logreg_1, se=T, col="blue")
'''

# remove outliers (all outliers)
gam.logreg_2 <- gam(I(wage>250)~ year + s(age,df=5) + education,
                    family=binomial, 
                    data=Wage)

stu_residuals_2 <- rstudent(gam.logreg_2)
plot(stu_residuals_2, type ="h")
outliers_index_2 <- which(a(stu_residuals_2)>3)
new_wage_data_2 <- wage_data[-outliers_index_2,]

# retrain my GAM (all outliers)
new_gam.logreg_2 <- gam(I(wage>250)~ year + s(age,df=5) + education,
                        family=binomial, 
                        data=new_wage_data_2)
par(mfrow=c(1,3))
plot(new_gam.logreg_2, se=T, col="blue")



# Take home questions
# 1. other non-linear model
gam_1 <- gam(wage~poly(year,6)+bs(age,5)+education,
             data=Wage)

# 2. mdeol selection in GAM for classification
index_remove <- wage_data$education == "1. < HS Grad"
new_wage_data <- wage_data[-index_remove,]
m1 <- gam(wage~poly(year,1)+education,
                   data=new_wage_data)
m2 <- gam(wage~poly(year,1)+poly(age,1)+education,
          data=new_wage_data)
m3 <- gam(wage~poly(year,1)+s(age,2)+education,
          data=new_wage_data)
m4 <- gam(wage~poly(year,1)+s(age,5)+education,
          data=new_wage_data)
m5 <- gam(wage~poly(year,1)+s(age,8)+education,
          data=new_wage_data)

anova(m1, m2, m3, m4, m5)


# 3. the basis function approach
#plot
train_data <- read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab5/train.csv",
                     sep = ",",
                     header = TRUE)

library("tibble")
train_data <- as_tibble(train_data)
par(mfrow=c(1,1))
plot(train_data$X, train_data$y)

# find best model
library(boot)
cv.error <- rep(0,26)

model <- glm(y ~ 1, 
             data = train_data)
cv.error[1] <- cv.glm(train_data, model, K= 10)$delta[1]



for (deg in 1:25) {
  model <- glm(y ~ poly(X, deg), 
               data = train_data)
  
  cv.error[deg+1] <- cv.glm(train_data,  model,  K = 10)$delta[1]
  
}
plot(cv.error, type="b")

optimal <- which.min(cv.error)
optimal
'''
# with cv.glm() nononononono
library(boot)
cv.error <- rep(0,26)
gam_mono_intercept <- gam(y ~ 1,
                          data = train_data)

cv.error[1] <- cv.glm(train_data,
                      gam_mono_intercept,
                     K = 10)$delta[1]

for (deg in 1:25) {
  gam_mono <- gam(y ~ poly(X,deg),
                  data=train_data)
  
  cv.error[i+1] <- cv.glm(train_data,  gam_mono,  K = 10)$delta[1]
  
}
plot(cv.error, type="b")
'''

# with cv.gam()
#install.packages("gamreg")
#library(gamreg)
#library(gam)
#library(mgcv)
#library(gamclass)

# use test data calculate MSE

best_model <- glm(y ~ poly(X, optimal-1), 
             data = train_data)

test_data <- read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab5/test.csv",
                         sep = ",",
                         header = TRUE)

test_data <- as_tibble(test_data)

predicted <- predict.lm(best_model, test_data)
mse <- mean((test_data$y-predicted)^2)
mse



install.packages("mpoly")
library("mpoly")

# 4. Laguerre polynomials
library("mpoly")

laguerre_function <- function(x, deg) {
  
  result <- matrix(1, nrow = nrow(x), ncol = deg+1)
  
  for (i in 1:deg) {
    formula <- laguerre(i, indeterminate = 'x')
    b <- parse(text=formula)
    
    contain <- matrix(0, nrow = nrow(x), ncol = 1)
    for (j in 2:length(b)) {
      degree <- j-1
      coefficient <- eval(b[j])[2]
      contain <-contain + coefficient * (x ^ degree)
      
    }
    contain <- contain + 1
    result[,1+i]<- contain
  }
  
  return(result)

}


# 5.
train_data <- read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab5/train.csv",
                         sep = ",",
                         header = TRUE)

library("tibble")
library(boot)
train_data <- as_tibble(train_data)

cv.error <- rep(0,20)

for (p in 1:20) {
  
  dfLagTrain <- data.frame(y = train_data$y, X = laguerre_function(data.matrix(train_data$X), p))
  
  glm.fit <- glm(y~.+0, data = dfLagTrain)
  
  cv.error[p] <- cv.glm(dfLagTrain, glm.fit, K= 10)$delta[1]
}

plot(cv.error, type="b")
optimal <- which.min(cv.error)
optimal

# calculate mse based on test data
test_data <- read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab5/test.csv",
                        sep = ",",
                        header = TRUE)
test_data <- as_tibble(test_data)
dfLagTrain <- data.frame(y = train_data$y, X = laguerre_function(data.matrix(train_data$X), optimal))


best_model <- glm(y~.+0, data = dfLagTrain)


X_for_test <- data.frame(X = laguerre_function(data.matrix(test_data$X), optimal))
predicted <- predict.lm(best_model, X_for_test)
mse <- mean((test_data$y-predicted)^2)
mse


# 7.dampened version of Laguerre
# install.packages("orthopolynom")


dam_laguerre_function <- function(x, deg) {
  
  result <- matrix(1, nrow = nrow(x), ncol = deg+1)
  
  for (i in 1:deg) {
    formula <- laguerre(i, indeterminate = 'x')
    b <- parse(text=formula)
    
    contain <- matrix(0, nrow = nrow(x), ncol = 1)
    for (j in 2:length(b)) {
      degree <- j-1
      coefficient <- eval(b[j])[2]
      contain <-contain + coefficient * (x ^ degree)
     
    }
    contain <- contain + 1
    contain <- contain * exp(-x/2)
    result[,1+i]<- contain
  }
  
  return(result)
  
}


dfLagTrain <- data.frame(y = train_data$y, X = dam_laguerre_function(data.matrix(train_data$X), optimal))
dampened_model <- glm(y~.+0, data = dfLagTrain)
X_for_test <- data.frame(X = dam_laguerre_function(data.matrix(test_data$X), optimal))
predicted <- predict.lm(dampened_model, X_for_test)
mse <- mean((test_data$y-predicted)^2)
mse

# Backfitting with multiple linear regression
# i
set.seed(1)
X1 = rnorm(100)
X2 = rnorm(100)
X3 = sin(X2)
eps = rnorm(100, sd = 0.01)
Y = 10 + 0.8 * X1 + 6 * X2 + 1.1 * X3 + eps
df <- data.frame(y=Y, X1=X1, X2=X2, X3=X3)

# ii
b1 <- 1

# iii
b1 <- 1
lmFix1 <- lm(y -b1 * X1 ~., data = df)
summary(lmFix1)


# iv
b2 <- 6.06695
lmFix2 <- lm(y -b2 * X2 ~., data = df)
summary(lmFix2)

# v
b3 <- 1.004
lmFix3 <- lm(y -b3 * X3 ~., data = df)
summary(lmFix3)

# vi
b0s <- rep(0,10)
b1s <- rep(0,10)
b2s <- rep(0,10)
b3s <- rep(0,10)

b1s[1] <- 1
for (i in 1:10) {
  b1 <- b1s[i]
  lmFix1 <- lm(y -b1 * X1 ~., data = df)
  b2s[i] <- coef(lmFix1)[2]
  lmFix2 <- lm(y -b2s[i] * X2 ~., data = df)
  b0s[i] <- coef(lmFix2)[1]
  b3s[i] <- coef(lmFix2)[3]
  lmFix3 <- lm(y -b3s[i] * X3 ~., data = df)
  
  b1s[i+1] <- coef(lmFix3)[2]
  
}


plot(b0s, type = "b")
plot(b1s, type = "b")
plot(b2s, type = "b")
plot(b3s, type = "b")

install.packages("knitr")
install.packages("rmarkdown")

