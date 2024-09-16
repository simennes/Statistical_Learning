
# -------------------------------- 1 ----------------------------

# a)
# Provide a detailed explanation of the algorithm that is used to fit a 
# regression tree. What is different for a classification tree?
#   
#   - For each variable, try different values to split the examples (all greater
#   go into one subset, all lower into another).
#   - Classify all in each subset to average
#   - Choose variable and value that reduces MSE the most.
#   - Keep going untill stopping criteria (e.g. dont split to less than five variables)
# 
# b)  
# What are the advantages and disadvantages of regression and classification trees?
#   
#   - Very intuitive (like a flow chart)
#   - Handles different variables (categorial, continuous etc.) automatically
# 
#   - Not so good preformance
#   - Not robust, high variance
# 
# c)  
# What is the idea behind bagging and what is the role of bootstap?
# How do random forests improve that idea?
#   
#   - Make lots of trees using bootstrap. Take the average of all the trees.
#     The idea is this should reduce the variance.
#   - Random forests improve this by not always allowing to split on all variables,
#     as this often causes vary similar trees. The reduced correlation should
#     improve preformance.
# 
# d)
# What is an out-of bag (OOB) error estimator and what percentage of 
# observations are included in an OOB sample? 
# (Hint: The result from RecEx5-Problem 4c can be used)
# 
#   - The OOB observations are those not chosen in a bagging example. On average,
#     this is 38% of the data. We can estimate the error using the OOB observations.
# 
# e)
# Bagging and Random Forests typically improve the prediction accuracy of a single
# tree, but it can be difficult to interpret, for example in terms of 
# understanding which predictors are how relevant. How can we evaluate the 
# importance of the different predictors for these methods?
#   
#   - We can produce variable importance plots using either decrease in node
#     impurity or radomization.

# -------------------------------- 2 ----------------------------

# a
library(ISLR)
data("Carseats")
set.seed(4268)
n = nrow(Carseats)
train = sample(1:n, 0.7*nrow(Carseats), replace = F)
test = -train
Carseats.train = Carseats[train,]
Carseats.test = Carseats[test,]

# b
library(tree)
tree.mod = tree(Sales~., data = Carseats.train)
tree.pred = predict(tree.mod, newdata = Carseats.test)
mean((Carseats.test$Sales - tree.pred)^2)

# c
cv.Carseats  = cv.tree(tree.mod) 
tree.min =  which.min(cv.Carseats$dev)
best = cv.Carseats$size[tree.min]
best

tree.pruned = prune.tree(tree.mod, best = best)
pruned.pred = predict(tree.pruned, newdata = Carseats.test)
mean((Carseats.test$Sales - pruned.pred)^2)

# d
library(randomForest)
dim(Carseats)
set.seed(4268)
bag.Carseats = randomForest(Sales~., data=Carseats.train, mtry = 10 , ntree = 500, importance = TRUE)
bag.pred = predict(bag.Carseats, newdata = Carseats.test)
mean((bag.pred - Carseats.test$Sales)^2)
importance(bag.Carseats)
varImpPlot(bag.Carseats)



# e
set.seed(4268)
forest.Carseats = randomForest(Sales~., data = Carseats.train, mtry = 3, ntree = 500, importance = T)
forest.pred = predict(forest.Carseats, newdata = Carseats.test)
mean((forest.pred - Carseats.test$Sales)^2)
varImpPlot(forest.Carseats)

# f
library(gbm)
r.boost=gbm(Sales~., Carseats.train,
            distribution= "gaussian",
            n.trees= 500 ,interaction.depth= 4, shrinkage = 0.1)
boost.pred = predict(r.boost, newdata = Carseats.test)
mean((boost.pred - Carseats.test$Sales)^2)


# g
N <- 50
by <- 10
mse.bag <- rep(0,N)
mse.rf <- rep(0,N)
x <- seq(from = 1, to = by*N, by = by)
j <- 1
for(i in x){
  forest.Carseats = randomForest(Sales~., data = Carseats.train, mtry = 3, ntree = i, importance = T)
  forest.pred = predict(forest.Carseats, newdata = Carseats.test)
  mse.rf[j] <- mean((forest.pred - Carseats.test$Sales)^2)
  
  bag.Carseats = randomForest(Sales~., data=Carseats.train, mtry = 10 , ntree = i, importance = T)
  bag.pred = predict(bag.Carseats, newdata = Carseats.test)
  mse.bag[j] <- mean((bag.pred - Carseats.test$Sales)^2)
  j <- j+1
}

plot(x, mse.rf, type="l", lwd=2, col="blue", ylab="MSE", xlab="ntree")
lines(x, mse.bag, col="red", lwd=2)
title("Effect of number of trees on error")

legend(N*0.77*by,mse.rf[1],legend=c("Random forest","Bagging"), col=c("blue","red"),lty=c(1,1), ncol=1)


# -------------------------------- 3 ----------------------------

library(kernlab)
data(spam)

set.seed(4268)
train = sample(1:nrow(spam), 0.7*nrow(spam))
test = -train
spam.train <- spam[train,]
spam.test <- spam[test,]

spam.tree <- tree(type~., data = spam.train)
summary(spam.tree)  
plot(spam.tree)  
text(spam.tree)

library(caret)
spam.pred <- predict(spam.tree, newdata=spam.test, type = "class")
(confMat <- confusionMatrix(spam.pred, reference = spam.test$type)$table)
1 - sum(diag(confMat))/sum(confMat[1:2, 1:2])

spam.cv <- cv.tree(spam.tree, FUN=prune.misclass)
plot(spam.cv$size, spam.cv$dev, type="b")
spam.prune <- prune.misclass(spam.tree, best=6)
plot(spam.prune)  
text(spam.prune)

prune.pred <- predict(spam.prune, newdata=spam.test, type="class")
(confMat <- confusionMatrix(prune.pred, reference = spam.test$type)$table)
1 - sum(diag(confMat))/sum(confMat[1:2, 1:2])

spam.bag <- randomForest(type~., data = spam.train, mtry=ncol(spam)-1, ntree=500, importance = T)
spam.bag.pred <- predict(spam.bag, newdata = spam.test)
(confMat <- confusionMatrix(spam.bag.pred, reference = spam.test$type)$table)
1 - sum(diag(confMat))/sum(confMat[1:2, 1:2])

spam.rf <- randomForest(type~., data = spam.train, mtry=as.integer(sqrt(ncol(spam)-1)), ntree=500, importance = T)
spam.rf.pred <- predict(spam.rf, newdata = spam.test)
(confMat <- confusionMatrix(spam.rf.pred, reference = spam.test$type)$table)
1 - sum(diag(confMat))/sum(confMat[1:2, 1:2])
varImpPlot(spam.rf)

spamboost = spam
spamboost$type = c()
spamboost$type[spam$type == "spam"] = 1
spamboost$type[spam$type == "nonspam"] = 0
spam.boost <- gbm(type~., data = spamboost[train,],
            n.trees= 5000 ,interaction.depth= 3, shrinkage = 0.001)
spam.boost.pred = predict(spam.boost, newdata = spamboost[test,], type="response")
spam.boost.pred <- ifelse(spam.boost.pred > 0.5, 1, 0)
boost.table <- table(spam.boost.pred, spamboost[test,]$type)
1 - sum(diag(boost.table))/sum(boost.table[1:2, 1:2])
