library(dplyr)
library(tibble)
install.packages("tree")
library(tree)
library(ISLR)
carseats <- as_tibble(Carseats) %>% mutate(High = as.factor(Sales > 8))
carseats


set.seed(10)
tree.carseats <- tree(High ~ . -Sales, carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

set.seed(10)
train <- sample(1 : nrow(carseats), 200)
carseats.test <- carseats[-train, ]
High.test <- carseats.test$High
tree.carseats <- tree(High ~. - Sales, carseats, subset = train)
tree.pred <- predict(tree.carseats, carseats.test, type = "class")
table(tree.pred, High.test)

summary(tree.carseats)

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats
plot(cv.carseats$size, cv.carseats$dev, type = 'b')

prune.carseats <- prune.misclass(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, carseats.test, type = "class")
table(tree.pred, High.test)

# problem1: bagging and random forests


# 3. regression tree
carseats <- select(carseats, -c(High))
set.seed(55)
train_id <- sample(1:nrow(carseats), 320)
carseats.train <- carseats[train_id,]
carseats.test <- carseats[-train_id,]

regression_tree <- tree(Sales ~.,
                        carseats)
plot(regression_tree)
pred_reg <- predict(regression_tree, carseats.test)
test_error_reg <- mean((carseats.test$Sales - pred_reg)^2)
test_error_reg

cv_reg <- cv.tree(regression_tree)

plot(cv_reg$size, cv_reg$dev, type = 'b')

best_size <- cv_reg$size[which.min(cv_reg$dev)]
best_size

regression_tree2 <- prune.tree(regression_tree, best = best_size)
plot(regression_tree2)
pred_reg2 <- predict(regression_tree2, carseats.test)
test_error_reg2 <- mean((carseats.test$Sales - pred_reg2)^2)
test_error_reg2


  
# 4.
install.packages("randomForest")
library(randomForest)
bag <- randomForest(Sales ~.,
                    data = carseats.train,
                    mtry = 10,
                    importance = TRUE)
pred_bag <- predict(bag, carseats.test)
test_error_bag <- mean((carseats.test$Sales - pred_bag)^2)
test_error_bag






# 6 -----------------------------------------------------------------------


test_mse <- rep(0,10)

for (i in 1:10) {
  
  forest <- randomForest(Sales~., 
                         data = carseats.train,
                         mtry = i,
                         importance = TRUE)
  
  pred_forest <- predict(forest, carseats.test)
  test_error_forest <- mean((carseats.test$Sales - pred_forest)^2)
  test_mse[i] <- test_error_forest
  
}
plot(test_mse)
best_m <- which.min(test_mse)
best_m

best_forest <- randomForest(Sales~., 
                       data = carseats.train,
                       mtry = best_m,
                       importance = TRUE)

varImpPlot(best_forest)


# problem 2 Boosting ------------------------------------------------------


library(ISLR)
library("tibble")
hitters <- na.omit(Hitters)
hitters$Salary <- log(hitters$Salary)

set.seed(20)

train_ind <- sample(1:nrow(hitters), 180)
hitters.train <- hitters[train_ind,]
hitters.test <- hitters[-train_ind,]

install.packages("gbm")
library(gbm)

set.seed(20)
training_error <- rep(0,10)
testing_error <- rep(0,10)

for (lambda in 1:10) {
  
  hitter_tree <- gbm(Salary ~., 
                     data = hitters.train, 
                     distribution = "gaussian",
                     n.trees = 1000,
                     shrinkage = 2^(-lambda))
  
  training_error[lambda] <- hitter_tree$train.error[1000]
  
  hitter.pred <- predict(hitter_tree, hitters.test)
  testing_error[lambda] <- mean((hitters.test$Salary - hitter.pred)^2)
}

x_axis <- rep(0,10)
for (i in 1:10) {
  x_axis[i] <- 2^-(i)
}
plot(x_axis, training_error, type = "b")
plot(x_axis, testing_error, type = "b")

# pick best lambda
best_lambda <- which.min(testing_error)

best_hitter_tree <- gbm(Salary ~., 
                   data = hitters, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   shrinkage = 2^(-best_lambda))


summary(best_hitter_tree)




