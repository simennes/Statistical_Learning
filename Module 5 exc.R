library(boot)
# GENERATE DATA; use a seed for reproducibility
set.seed(4268)
n = 100  #number of observations
p = 5000  #number of predictors
d = 10  #top correlated predictors chosen

# Generating predictor data
xs = matrix(rnorm(n * p, 0, 1), ncol = p, nrow = n)  #simple way to to uncorrelated predictors
dim(xs)  # n times p
xs[1:10, 1:10]

# Generate class labels independent of predictors - so if all
# classifies as class 1 we expect 50% errors in general
ys = c(rep(0, n/2), rep(1, n/2))  #now really 50% of each
table(ys)


#---------WRONG CV----------
corrs = apply(xs, 2, cor, y = ys)
hist(corrs)


selected = order(corrs^2, decreasing = TRUE)[1:d]

data = data.frame(ys, xs[, selected])


logfit <- glm(ys ~ ., family = "binomial", data = data)
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)
kfold <- 10
cvres <- cv.glm(data = data, cost = cost, glmfit = logfit, K = kfold)
cvres$delta

#---------CORRECT CV-------
reorder <- sample(1:n, replace = FALSE)
validclass <- NULL
for (i in 1:kfold) {
  neach <- n/kfold
  trainids <- setdiff(1:n, (((i - 1) * neach + 1):(i * neach)))
  traindata <- data.frame(xs[reorder[trainids], ], ys[reorder[trainids]])
  validdata <- data.frame(xs[reorder[-trainids], ], ys[reorder[-trainids]])
  colnames(traindata) <- colnames(validdata) <- c(paste("X", 1:p),
                                                  "y")
  foldcorrs <- apply(traindata[, 1:p], 2, cor, y = traindata[, p +
                                                               1])
  selected <- order(foldcorrs^2, decreasing = TRUE)[1:d]  # Select top d correlated 
  data <- traindata[, c(selected, p + 1)]
  trainlogfit <- glm(y ~ ., family = "binomial", data = data)
  pred <- plogis(predict.glm(trainlogfit, newdata = validdata[, selected]))
  validclass <- c(validclass, ifelse(pred > 0.5, 1, 0))
}
table(ys[reorder], validclass)
1 - sum(diag(table(ys[reorder], validclass)))/n



#----------------TASK 4--------------

N <- 1000
n <- 1000
xs <- 1:n

total <- 0
for(i in 1:N){
  count <- 0
  for(j in 1:n){
    if(sample(xs,1)==1){
      count <- count + 1
    }
  }
  if(count >= 1){
    total <- total + 1
  }
}

print(total/N)

#----------TASK 6-----------
library(car)?
library(boot)
SLID <- na.omit(SLID)
n <- dim(SLID)[1]
SLID.lm <- lm(wages ~ ., data = SLID)
summary(SLID.lm)$coeff["age", ]

summary(SLID)

B <- 100
boot.fn <- function(data, index) {
  return(coef(lm(wages ~ ., data = SLID, subset = index)))
}
boot(SLID,boot.fn,B)

coeffs <- rep(0.0,B)
for(i in 1:B){
  indices <- sample(1:n,n,replace = T)
  new_coef <- coef(lm(wages ~ ., data = SLID, subset=indices))
  coeffs[i] <- new_coef["age"]
}
sd(coeffs)
quantile(coeffs, c(0.025,0.975))
