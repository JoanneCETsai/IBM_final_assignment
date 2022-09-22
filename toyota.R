# data loading and cleaning

corollas<-read.table(file = "/Users/chiaentsai/Desktop/Data Mining/lab/lab3/ToyotaCorolla-1.csv",
                     sep = ",",
                     header = TRUE)

library("tibble")
corollas<-as_tibble(corollas)
corollas<-corollas[, c("Price", "Age_08_04", "KM")]

str(corollas)
typeof(corollas)

which(is.na(corollas))
sum(is.na(corollas)) # make sure there is no missing data

set.seed(1)
train_id<-sample(nrow(corollas),nrow(corollas)/2)

lm.fit = lm(Price~ Age_08_04 + KM, data = corollas, subset = train_id)

predicted_values <- predict.lm(lm.fit, corollas[-train_id,])
mse <- mean((corollas[-train_id,]$Price-predicted_values)^2)
mse


# mse for models with degree up to 5 - validation set
eval_lm <- function(deg){
  poly_fit <- lm(Price ~ poly(Age_08_04, degree = deg) + poly(KM, degree = deg), 
                 data = corollas, 
                 subset = train_id)
  poly_predicted <- predict.lm(poly_fit, corollas[-train_id,])
  ploy_mse <- mean((corollas[-train_id,]$Price-poly_predicted)^2)
  return(ploy_mse)
}

for (i in 1:5) {
  print(paste("mse of degree", i, ":", eval_lm(i)))
}



'''
#Just my test
#use poly() and raw=TRUE

eval_lm_rawTRUE <- function(deg){
  poly_fit <- lm(Price ~ poly(Age_08_04, KM, degree = deg, raw=TRUE), data = corollas, subset = train_id)
  poly_predicted <- predict.lm(poly_fit, corollas[-train_id,])
  ploy_mse <- mean((corollas[-train_id,]$Price-poly_predicted)^2)
  return(ploy_mse)
}

for (i in 1:5) {
  print(paste("(raw=TURE) mse of degree", i, ":", eval_lm_rawTRUE(i)))
}
'''



# my codes - LOOCV
for (deg in 1:5) {
  glm.fit <- glm(Price ~ poly(Age_08_04, degree = deg) + poly(KM, degree = deg), 
                 data = corollas)
  cv.error[i] <- cv.glm(corollas, glm.fit)$delta[1]
  
}
plot(cv.error, type="b")


# codes on pdf, basically theyre the same - LOOCV

cv.error<-rep(0,5)
for (i in 1:5) {
     glm.fit <- glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
   cv.error[i] <- cv.glm(corollas, glm.fit)$delta[1] }
plot(cv.error, type="b")

# cubic model
glm.fit_cubic <- glm(Price~poly(Age_08_04, 3)+poly(KM, 3), data = corollas)
summary(glm.fit_cubic)


# k- fold cross- validation
par(mfcol=c(2,3))
for (k in 5:10) {
  cv.error<-rep(0,5)
  for (i in 1:5) {
    glm.fit <- glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
    cv.error[i] <- cv.glm(corollas, glm.fit, K= k)$delta[1] }
  
  plot(cv.error, type="b")
  title(paste("K=", k))
  
}

# runnung time - validation set
ptm = proc.time()
poly_fit <- lm(Price ~ poly(Age_08_04, degree = deg) + poly(KM, degree = deg), 
               data = corollas, 
               subset = train_id)
poly_predicted <- predict.lm(poly_fit, corollas[-train_id,])
ploy_mse <- mean((corollas[-train_id,]$Price-poly_predicted)^2)
proc.time() - ptm

# runnung time - LOOCV
ptm = proc.time()
glm.fit <- glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
cv.error[i] <- cv.glm(corollas, glm.fit)$delta[1]
proc.time() - ptm

# running time - K fold 
ptm = proc.time()
glm.fit <- glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
cv.error[i] <- cv.glm(corollas, glm.fit, K= k)$delta[1]
proc.time() - ptm


# bias variance tradeoff - cars

cars
cars_data <- tibble(cars)

set.seed(1997)

train_id <- sample(nrow(cars_data), 2 * nrow(cars_data) / 3)

# no intercept linear regression
lm_no_intercept = lm(dist~ speed + 0, data = cars_data, subset = train_id)
predicted_no_intercept_test <- predict.lm(lm_no_intercept, cars_data[-train_id,])
predicted_no_intercept_train <- predict.lm(lm_no_intercept, cars_data[train_id,])
test_mse_no_intercept <- mean((cars_data[-train_id,]$dist - predicted_no_intercept_test)^2)
train_mse_no_intercept <- mean((cars_data[train_id,]$dist - predicted_no_intercept_train)^2)
train_mse_no_intercept
test_mse_no_intercept

# intercept linear regression model
lm_intercept <- lm(dist~ speed, data = cars_data, subset = train_id)
predicted_intercept_test <- predict.lm(lm_intercept, cars_data[-train_id,])
predicted_intercept_train <- predict.lm(lm_intercept, cars_data[train_id,])
test_mse_intercept <- mean((cars_data[-train_id,]$dist - predicted_intercept_test)^2)
train_mse_intercept <- mean((cars_data[train_id,]$dist - predicted_intercept_train)^2)
train_mse_intercept
test_mse_intercept

# quadratic (non linear) model
qua_fit <- lm(dist ~ poly(speed, degree = 3), 
               data = cars_data, 
               subset = train_id)

predicted_qua_test <- predict.lm(qua_fit, cars_data[-train_id,])
predicted_qua_train <- predict.lm(qua_fit, cars_data[train_id,])
test_mse_qua <- mean((cars_data[-train_id,]$dist - predicted_qua_test)^2)
train_mse_qua <- mean((cars_data[train_id,]$dist - predicted_qua_train)^2)
train_mse_qua
test_mse_qua



# Part 2. Logistic regression

set.seed(1)
n <- 1000
x1 <- runif(n)
x2 <- runif(n, -2, 1)
z <- (x1-0.2)*(x1-0.5)*(x1-0.9) * 25 - x2*(x2+1.2)*(x2-0.8) + rnorm(n)/3
y <- as.integer(z>0)
plot(x1, x2, col=c("red", "blue")[y+1])
df <- data.frame(x1,x2,y)

library("tibble")
df <- tibble(df)

# bootstrap
library(boot)

boot_fn <- function(data, train_id){
  
  logistic_fit <- glm(y ~ x1 + x2, 
                      data = df, 
                      subset = train_id,
                      family = binomial)
  cv.error <- cv.glm(df, logistic_fit)$delta[1]
  return(cv.error)
  
}

results <- boot(data=df, statistic = boot_fn, R=10)
sum(results$t)/10


# Logistic regression with cross- validation

cost_fn <- function(r, pi = 0){
  mean(abs(r- pi) > 0.5)
}


seed(43)
train_id <- sample(nrow(df), 9 * nrow(df) / 10)
cv.error<-rep(0,5)
for (p in 1:5) {
  glm.fit <- glm(y ~ poly(x1, p) + poly(x2, p), 
                 data = df[train_id,], 
                 family = binomial, 
                 )
  cv.error[p] <- cv.glm(df[train_id,], glm.fit, cost = cost_fn, K=10)$delta[1] 
  
}

plot(cv.error, type="b")



# I choose 5
glm.fit <- glm(y ~ poly(x1, 5) + poly(x2, 5), 
               data = df[train_id,], 
               family = binomial, 
)
error <- cv.glm(df[-train_id,], glm.fit, cost = cost_fn)$delta[1] 
error

