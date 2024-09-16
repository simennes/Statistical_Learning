#------------------- 2 ---------------------

library(ISLR)
library(GGally)
library(ggplot2)
Credit_slim <- Credit[c("Balance", "Age", "Cards", "Education", "Income", "Limit", "Rating")]

ggpairs(Credit_slim)

#------------------- 3 ---------------------
library(leaps)
set.seed(1)
credit_data <- subset(Credit, select=-c(ID))
train_frac <- 0.75
train_indices <- sample(1:nrow(credit_data), nrow(credit_data)*train_frac)
test_indices <- (-train_indices)

train_data <- credit_data[train_indices, ]
test_data <- credit_data[test_indices, ]
rownames(train_data) <- 1:nrow(train_data) 
rownames(test_data) <- 1:nrow(test_data) 
train_data
regfit_sum <- summary(regsubsets(Balance ~ .,train_data,nvmax=11))
regfit_sum$cp
regfit_sum$bic
regfit_sum$adjr2
regfit_sum

#cp: 6 vars, bic: 5 vars, adjr2: 7 vars

library(boot)

k <- 10
set.seed(1)
folds <- sample(factor(rep(1:k, length.out=nrow(train_data))))
errors <- matrix(rep(0,k*11),ncol=11)
#i <- 1
for(i in 1:k){
  lo <- folds %in% i
  best <- regsubsets(Balance~., data=train_data[which(!lo),],nvmax=11)
  best_sum <- summary(best)
  #j <- 1
  for(j in 1:11){
    cols <- c(best_sum$which[j,])[2:12]
    curr_data <- c()
    curr_data <- data.frame(train_data[which(!lo),cols])
    curr_data$Balance <- train_data$Balance[which(!lo)]
    
    test_data <- c()
    test_data <- data.frame(train_data[which(lo),cols])
    test_data$Balance <- train_data$Balance[which(lo)]
    if(j == 1){
      test_names <- colnames(test_data)
      test_names[1] <- names(cols[which(cols)])
      colnames(test_data) <- test_names
      curr_names <- colnames(curr_data)
      curr_names[1] <- names(cols[which(cols)])
      colnames(curr_data) <- curr_names
    }
    
    model <- lm(Balance~., data=curr_data)
    errors[i,j] <- mean((predict(model,newdata=test_data) - test_data$Balance)^2)
  }
}
mean_cv <- c()
for(i in 1:11){
  mean_cv <- c(mean_cv,mean(errors[,i]))
}
plot(1:11,mean_cv,type="b")

# Looks like going beyond 5-6 variables is useless (ish same for step 1)

#----------------------------- 4 --------------------
best_fwd <- regsubsets(Balance~., data=train_data,nvmax=11, method="forward")
best_bck <- regsubsets(Balance~., data=train_data,nvmax=11, method="backward")
best_hyb <- regsubsets(Balance~., data=train_data,nvmax=11, method="seqrep")

summary(best_fwd)
summary(best_bck)
summary(best_hyb)

#-------------------------- 5 -----------------------
library(glmnet)
x <- model.matrix(Balance~., train_data)[,-1]
y <- train_data$Balance
x_test <- model.matrix(Balance~., test_data)[,-1]
y_test <- test_data$Balance

linmod <- lm(Balance~.,data = train_data)
linmod_pred <- predict(linmod,newdata = test_data)
linmod_square_error <- mean((linmod_pred-y_test)^2)

#---

ridge <- glmnet(x,y,alpha=0)
ridge_cv <- cv.glmnet(x,y,alpha=0)
plot(ridge_cv)

lambda_min <- ridge_cv$lambda.min

ridge_pred = predict(ridge,s=lambda_min,newx=x_test)
ridge_square_error <- mean(as.numeric((ridge_pred-y_test)^2))

#----------------------- 6 -------------------
lasso <- glmnet(x,y,alpha=1)
lasso_cv <- cv.glmnet(x,y,alpha=1)
plot(lasso_cv)

lambda_min <- lasso_cv$lambda.min

lasso_pred = predict(lasso,s=lambda_min,newx=x_test)
lasso_square_error <- mean(as.numeric((lasso_pred-y_test)^2))

# Compare:
linmod_square_error
ridge_square_error
lasso_square_error

#------------------------- 7 ---------------
x <- model.matrix(Balance~.,credit_data)[,-1]

credit_pca <- prcomp(x, center = TRUE, scale. = TRUE)

print(credit_pca)
plot(credit_pca, type="l")
summary(credit_pca)

#------------------------- 8 -------------
library(pls)
pcr.fit <- pcr(Balance~., data = train_data,
                  scale = TRUE , validation = "CV")

pcr.pred <- predict(pcr.fit, test_data, ncomp = 10)
pcr_square_error <- mean((pcr.pred - y_test)^2)

#------------------------- 9 --------------
pls.fit <- plsr(Balance~., data = train_data, scale = T, validation="CV")
validationplot(pls.fit, "MSEP")

pls.pred <- predict(pls.fit, test_data, ncomp=4)
pls_square_error <- mean((pls.pred - y_test)^2)

# Compare:
linmod_square_error
ridge_square_error
lasso_square_error
pcr_square_error
pls_square_error