##   x1 x2 y
## 1  3  3 A
## 2  2  0 A
## 3  1  1 A
## 4  0  1 B
## 5 -1  0 B
## 6  2  1 B
## 7  1  0 B

#---------------OPPGAVE 1----------------

df <- data.frame(c(3,2,1,0,-1,2,1), c(3,0,1,1,0,1,0),c("A","A","A","B","B","B","B"))
colnames(df) = c("x1","x2","y")

distance <- function(p1,p2){
  return(sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2))
}

testpoint <- c(1,2)
for(i in 1:length(df$x1)){
  print(distance(testpoint,c(df[i,1],df[i,2])))
}

library(ggplot2)
#--------------------------

classify <- function(p, K){
  n <- length(df$x1)
  distances <- c()
  for(i in 1:n){
    distances <- c(distances, distance(p,c(df[i,1],df[i,2])), df[i,3])
  }
  distances <- matrix(distances,ncol=2,byrow=T)
  distances <- distances[order(distances[,1]),]
  
  count <- c(0,0)
  for (i in 1:K){
    if(distances[i,2]=="A"){
      count <- count + c(1,0)
    }
    else{
      count <- count + c(0,1)
    }
  }
  if(count[1]>=count[2]){
    return("A")
  }
  return("B")
}

classify_test_grid <- function(xlim, ylim, by, K){
  x0 <- seq(from=xlim[1], to=xlim[2], by=by)
  y0 <- seq(from=ylim[1], to=ylim[2], by=by)
  nx <- length(x0)
  ny <- length(y0)
  
  xs <- c(1:(nx*ny))
  ys <- c(1:(nx*ny))
  
  for (i in 1:nx){
    xs[((i-1)*ny+1):(i*ny)] <- rep(x0[i],ny)
    ys[((i-1)*ny+1):(i*ny)] <- y0
  }
  
  classes <- c()
  for (x in x0){
    for (y in y0){
      classes <- c(classes,classify(c(x,y),K))
    }
  }
  df <- data.frame(xs,ys,classes)
  colnames(df) <- c("x", "y", "resp")
  return(df)
}

#-------------------------------

K <- 3
df_test <- classify_test_grid(c(-1,3), c(0,3), 0.1, K)

ggplot() +
  geom_point(data=df_test, aes(x,y,color=resp)) +
  geom_point(data=df, aes(x1,x2, color=y, shape=y, size = 2)) +
  labs(title = paste("KNN for K =",paste0(K)))

#----------------------OPPGAVE 2-------------------

#a) Use known formula 
#(see https://htmlpreview.github.io/?https://github.com/stefaniemuff/statlearning/blob/master//4Classif/4Classif.html#Confusion_matrix_for_the_synthetic_example)

#b) LDA assumes gaussian distributed and common covariance matrices.
#   Classify to real if discriminant of real is greater than fake.

#c)

x <- matrix(c(214.0,140.4),ncol=1)

ng <- 500
nf <- 500
n <- nf + ng
K <- 2
pi_g <- 0.5
pi_f <- 0.5

mu_g <- matrix(c(214.97,141.52),ncol=1)
mu_f <- matrix(c(214.82,139.45),ncol=1)

sigma_g <- matrix(c(0.1502,0.0055,0.0055,0.1998),ncol=2)
sigma_f <- matrix(c(0.1240,0.0116,0.0116,0.3112),ncol=2)

#----LDA-----

sigma <- ((ng-1)/(n-K))*sigma_g + ((nf-1)/(n-K))*sigma_f

d_g <- t(x) %*% solve(sigma) %*% mu_g - 0.5*t(mu_g) %*% solve(sigma) %*% mu_g + log(pi_g)
d_f <- t(x) %*% solve(sigma) %*% mu_f - 0.5*t(mu_f) %*% solve(sigma) %*% mu_f + log(pi_f)
print(c(d_g, d_f))
  
if(d_g>=d_f){
  print("GENUINE")
} else{
  print("FAKE")
}

#-----QDA------

d_g <- -0.5*t((x-mu_g))%*%solve(sigma_g)%*%(x-mu_g)-0.5*log(det(sigma_g))+log(pi_g)
d_f <- -0.5*t((x-mu_f))%*%solve(sigma_f)%*%(x-mu_f)-0.5*log(det(sigma_f))+log(pi_f)

print(c(d_g, d_f))

if(d_g>=d_f){
  print("GENUINE")
} else{
  print("FAKE")
}


#-----------------------Problem 3------------------------

# o = p/(1-p) --> p = o/(1+o)
o <- 0.37
print("Fraction:")
print(o/(1+o))
p <- 0.27
print("Odds:")
print(p/(1-p))

#-----------------------Problem 4------------------------

#a)
x1 <- 40            # hours studied
x2 <- 3.5           # GPA

beta0 <- -6
beta1 <- 0.05
beta2 <- 1

eta <- beta0 + beta1*x1 + beta2*x2
p <- exp(eta)/(1+exp(eta))

print(p)

#b) Rearrange formula for x1:

print((log(0.5/(1-0.5))-x2*beta2-beta0)/beta1)

#-----------------------Problem 5------------------------

#a) 
# Sensitivity: Correct positive/Total positive
# Specificity: Correct negative/Total negative

#b)

# Plot Sense(y) vs 1-sense(x) for different thresholds
# Useful to investigate which cutoff is best

# c) AUC is area under ROC and the greater the score, the closer the
#    ROC tends to the top left corner. Therfore one would probably prefeer q(x)

#------------------Problem 6--------------------------

#a)
library(ISLR)
library(GGally)
data("Weekly")
summary(Weekly)
ggpairs(Weekly)

#b)
log_reg_dir <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                   family="binomial", data=Weekly)

summary(log_reg_dir)

# Looks like lag 2 is the best predictor

#c)
glm.probs_Weekly = predict(log_reg_dir, type = "response")
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
table(glm.preds_Weekly, Weekly$Direction)

# Very often predicts up when it will actually go down

#d)
Weekly_trainID = (Weekly$Year < 2009)
Weekly_train = Weekly[Weekly_trainID, ]
Weekly_test = Weekly[!Weekly_trainID, ]

log_reg_dir2 <- glm(Direction ~ Lag2, family="binomial", data=Weekly_train)

glm.probs_Weekly = predict(log_reg_dir2, type = "response", newdata = Weekly_test)
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
table(glm.preds_Weekly, Weekly_test$Direction)

# Pretty much same tendency

#e)
library(MASS)
lda_dir <- lda(Direction ~ Lag2, data=Weekly_train)

lda.preds_Weekly = predict(lda_dir,newdata = Weekly_test)$class
table(lda.preds_Weekly, Weekly_test$Direction)

#f)
qda_dir <- qda(Direction ~ Lag2, data=Weekly_train)

qda.preds_Weekly = predict(qda_dir,newdata = Weekly_test)$class
table(qda.preds_Weekly, Weekly_test$Direction)

#g)
library(class)
k <- 1
knn.train = as.matrix(Weekly_train$Lag2)
knn.test = as.matrix(Weekly_test$Lag2)

set.seed(123)
yourKNNmodel = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k = 1, prob = T)
table(yourKNNmodel, Weekly_test$Direction)

#h)
# knn error:
K = 30
knn.error = rep(NA, K)

set.seed(234)
for (k in 1:K) {
  knn.pred = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k = k)
  knn.error[k] = mean(knn.pred != Weekly_test$Direction)
}
knn.error.df = data.frame(k = 1:K, error = knn.error)
ggplot(knn.error.df, aes(x = k, y = error)) + geom_point(col = "blue") + geom_line(linetype = "dotted")

k <- 12
knn.train = as.matrix(Weekly_train$Lag2)
knn.test = as.matrix(Weekly_test$Lag2)

set.seed(123)
yourKNNmodel = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k = k, prob = T)
table(yourKNNmodel, Weekly_test$Direction)

#Correct preds: 0.596

#i) Logistic regression seems best

#j)

#install.packages('plotROC')
#install.packages('pROC')
#library(pROC)
#library(plotROC)
lda.preds_Weekly = predict(lda_dir, newdata = Weekly_test)$posterior
qda.preds_Weekly = predict(qda_dir, newdata = Weekly_test)$posterior
# get the probabilities for the classified class
knn12.Weekly_prob = attributes(yourKNNmodel)$prob
# since we want the probability for Up, we need to take 1-p for the elements that gives probability for Down
down = which(yourKNNmodel == "Down")
knn12.Weekly_prob[down] = 1 - knn12.Weekly_prob[down]
library(pROC)
print("GLM")
glm.probs_Weekly
lda.preds_Weekly[, 2]
glmroc = roc(response = Weekly_test$Direction, predictor = glm.probs_Weekly, direction = "<")
ldaroc = roc(response = Weekly_test$Direction, predictor = lda.preds_Weekly[, 2], direction = "<")
qdaroc = roc(response = Weekly_test$Direction, predictor = qda.preds_Weekly[, 2], direction = "<")
knnroc = roc(response = Weekly_test$Direction, predictor = knn12.Weekly_prob, direction = "<")

# or use ggplot2
dat = data.frame(Direction = Weekly_test$Direction, glm = glm.probs_Weekly, lda = lda.preds_Weekly[, 2], qda = qda.preds_Weekly[, 2],
                 knn = knn12.Weekly_prob)
dat_long = melt_roc(dat, "Direction", c("glm", "lda", "qda", "knn"))
ggplot(dat_long, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = F) + xlab("1-Specificity") + ylab("Sensitivity")
# glm is very similar to lda, so the roc-curve for glm is not shown.


# AUC:
auc(knnroc)
auc(glmroc)
