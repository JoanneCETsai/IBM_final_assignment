# process the data
install.packages("ISLR")
library(ISLR)
library("tibble")
Hitters
df<-as_tibble(Hitters)

# remove missing data
df <- na.omit(df)

# fit model before remove outliers
lm.fit_before = lm(formula = Salary~., data = df)
summary(lm.fit_before)

# remove outliers

stu_residuals <- rstudent(lm.fit_before)
plot(stu_residuals, type ="h")
outliers_index <- which(abs(stu_residuals)>3)
df.post <- df[-outliers_index,]

# fit model after remove outliers
lm.fit_after = lm(formula = Salary~., data = df.post)
summary(lm.fit_after)

# fit model without intercept
lm.fit_nointercept = lm(formula = Salary~. -1, data = df.post)
summary(lm.fit_nointercept)


# 1 subset selection methods

library(leaps)
regfit.full <- regsubsets(Salary~., data = df.post, nvmax = 19, intercept = F)
reg.summary <-summary(regfit.full)
reg.summary

# plot adjusted R squared score and BIC score
plot(reg.summary$adjr2)
which.max(reg.summary$adjr2)


plot(reg.summary$bic)
which.min(reg.summary$bic)

# plot cp score
plot(reg.summary$cp)
which.min(reg.summary$cp)


# Ridge and Lasso

y <- df.post$Salary
x <- model.matrix(Salary~., df.post)[, -1]  # -1 means no intercept

install.packages("glmnet")
install.packages("Matrix")
install.packages("Rcpp")
library("glmnet")

grid <- 10 ^ seq(10, -2, length = 100)  # lambda sequence
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid, intercept = FALSE)
dim(coef(ridge.mod))

ridge.mod$beta

l2norm <- rep(0,100)
for (i in 1:100) {
  l2norm[i] <- sqrt(sum(ridge.mod$beta[,i]^2))
  
}

plot(log10(grid), l2norm)


# Split the data into training data set and test data set.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
y.train <- y[train]
y.test <- y[-train]

# Model fit on the training set - Ridge
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid,
                    intercept=FALSE)
# Use 10-fold cross-validation to choose lambda - Ridge
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], nfolds = 10, lambda=grid,
                      alpha=0, intercept=FALSE)

# plot the cross validation error vs the log of tunung parameter -Ridge
cross_validation_error <- cv.out$cvm
plot(log10(grid), cross_validation_error)

optimal <- grid[which.min(cross_validation_error)]

# using optimal tuning parameter, calculate mse on the test set -Ridge
optimal <- grid[which.min(cross_validation_error)]
ridge.opt <- glmnet(x[train,], y[train], alpha=0, lambda = optimal,
                    intercept=FALSE)
predicted_value <- predict(ridge.opt, x[-train,])
mse_ridge <- mean((y[-train]-predicted_value)^2)
mse_ridge



#  Model fit on the training set - LASSO
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid,
                    intercept=FALSE)

# Use 10-fold cross-validation to choose lambda - LASSO
cv.out <- cv.glmnet(x[train,], y[train], alpha=1, nfolds = 10, lambda=grid, 
                    intercept=FALSE)

# plot the cross validation error vs the log of tunung parameter - LASSO
cross_validation_error <- cv.out$cvm
plot(log10(grid), cross_validation_error)

optimal <- grid[which.min(cross_validation_error)]
optimal

# using optimal tuning parameter, calculate mse on the test set - LASSO
optimal <- grid[which.min(cross_validation_error)]
lasso.opt <- glmnet(x[train,], y[train], alpha=1, lambda = optimal,
                    intercept=FALSE)
predicted_value <- predict(lasso.opt, x[-train,])
mse_lasso <- mean((y[-train]-predicted_value)^2)
mse_lasso

lasso.opt$beta


# build a predict function for regsubsets
predict.regsubsets <- function(regfit, newdata, id) {
  mat <- model.matrix(formula(Salary~. + 0), newdata)
  coefi <- coef(regfit, id=id)
  xvars <- names(coefi)
  as.matrix(mat[, xvars]) %*% coefi
}


# split the data and write for loop
# k = 10
k <- 10
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F)
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)

# k = 5
k <- 5
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F)
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)



# repeat the experiment using forward
# k = 10
k <- 10
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F, method = "forward")
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)

# k = 5
k <- 5
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F, method = "forward")
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)




# repeat the experiment using backward
# k = 10
k <- 10
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F, method = "backward")
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)

# k = 5
k <- 5
set.seed (1)
folds <- sample(1:k,nrow(df.post),replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = df.post[folds!=j,], nvmax = 19, intercept = F, method = "backward")
  
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, df.post[folds==j, ], id = i)
    cv.errors[j, i] <- mean((df.post$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors)
which.min(mean.cv.errors)




# inspect the Lasso’s tendency to promote sparse solutions
X <- matrix( rnorm(500*600,mean=0,sd=1), 500, 600) 

beta <- matrix( rnorm(600*1,mean=0,sd=1), 600, 1)
beta <- as.vector(beta)
beta[1:300] <- 0 

epsilon <- matrix( rnorm(500*1,mean=0,sd=1), 500, 1) 
y <- X %*% beta +epsilon

# split data into 6:4
train <- sample(1:nrow(X), nrow(X) * 6/10)
grid <- 10 ^ seq(10, -2, length = 100) 
lasso.model <- glmnet(X[train,], y[train], alpha=1, lambda=grid,
                    intercept=FALSE)


# Use 10-fold cross-validation to analyze 
set.seed(1)
cv.out <- cv.glmnet(X[train,], y[train], alpha=1, nfolds = 10, lambda=grid, 
                    intercept=FALSE)

cross_validation_err <- cv.out$cvm
plot(log10(grid), cross_validation_err)

optimal <- grid[which.min(cross_validation_err)]
optimal

lasso.opt <- glmnet(X[train,], y[train], alpha=1, lambda = optimal,
                    intercept=FALSE)
predicted_value <- predict(lasso.opt, X[-train,])
mse_lasso_10 <- mean((y[-train]-predicted_value)^2)
mse_lasso_10

lasso.opt$beta
sum(lasso.opt$beta ==0)



# Use 5-fold cross-validation to analyze 
set.seed(1)
cv.out <- cv.glmnet(X[train,], y[train], alpha=1, nfolds = 5, lambda=grid, 
                    intercept=FALSE)

cross_validation_err <- cv.out$cvm
plot(log10(grid), cross_validation_err)

optimal <- grid[which.min(cross_validation_err)]
optimal

lasso.opt <- glmnet(X[train,], y[train], alpha=1, lambda = optimal,
                    intercept=FALSE)
predicted_value <- predict(lasso.opt, X[-train,])
mse_lasso_5 <- mean((y[-train]-predicted_value)^2)
mse_lasso_5

lasso.opt$beta
sum(lasso.opt$beta ==0)

