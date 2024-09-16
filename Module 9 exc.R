set.seed(1)
x <- matrix ( rnorm (20 * 2), ncol = 2)
y <- c( rep (-1, 10), rep (1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot (x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))
library (e1071)
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

tune.out <- tune(svm , y~., data = dat , kernel = "linear",
                  ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(tune.out$best.model)


xtest <- matrix ( rnorm (20 * 2), ncol = 2)
ytest <- sample (c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest , y = as.factor(ytest))

ypred <- predict(bestmod , testdat)
table (predict = ypred , truth = testdat$y)

svmfit <- svm ( y ~ ., data = dat , kernel = "linear",
                  cost = .01, scale = FALSE)
ypred <- predict (svmfit , testdat)
table (predict = ypred , truth = testdat$y)



x[y == 1, ] <- x[y == 1, ] + 0.5
plot (x, col = (y + 5) / 2, pch = 19)

dat <- data.frame (x = x, y = as.factor (y))
svmfit <- svm ( y ~ ., data = dat , kernel = "linear",
                  cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

# ---------------------------------- 3 --------------------------------------


# code taken from video by Trevor Hastie
set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1, 1), c(10, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = y + 3, pch = 19, xlab = expression(X[1]), ylab = expression(X[2]))

dat <- data.frame(x, y = as.factor(y))

# a)

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)

make.grid <- function(x, n = 75) {
  # takes as input the data matrix x
  # and number of grid points n in each direction
  # the default value will generate a 75x75 grid
  grange <- apply(x, 2, range) # range for x1 and x2
  # Sequence from the lowest to the upper value of x1
  x1 <- seq(from = grange[1, 1], to = grange[2, 1], length.out = n)
  # Sequence from the lowest to the upper value of x2
  x2 <- seq(from = grange[1, 2], to = grange[2, 2], length.out = n)
  # Create a uniform grid according to x1 and x2 values
  expand.grid(X1 = x1, X2 = x2)
}


x <- as.matrix(dat[, c("X1", "X2")])
xgrid <- make.grid(x)
ygrid <- predict(svmfit, xgrid)
plot(xgrid, col = c("pink", "lightblue")[as.numeric(ygrid)], pch=20, cex=1)
points(x, col = c("red", "darkblue")[as.numeric(svmfit$fitted)], pch=16)
points(x[svmfit$index, ], col = c("red", "darkblue")[as.numeric(svmfit$fitted)], pch=17, cex=1.5)

beta <- drop(t(svmfit$coefs) %*% x[svmfit$index, ])
beta0 <- svmfit$rho
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty=2)





# --------------------------------- 4-------------------------------


load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))
#names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)
plot(x, col = y + 1, pch = 19, xlab = expression(X[1]), ylab = expression(X[2]))

dat <- data.frame(y = factor(y), x)
r.cv <- tune(svm,
             factor(y) ~ .,
             data = dat,
             kernel = "radial",
             ranges = list(cost = c(1e-3,1e-2,0.1,1,10,100,1e3),
             gamma = c(1e-2,0,1,1,10,100,1e3)))

xgrid <- make.grid(x)
ygrid <- predict(r.cv$best.model, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.4)
points(x, col = y + 1, pch = 19)

# decision boundary
func <- predict(r.cv$best.model, xgrid, decision.values = TRUE)
func <- attributes(func)$decision
contour(unique(xgrid[, 1]),
        unique(xgrid[, 2]),
        matrix(func, 75, 75),
        level = 0,
        add = TRUE) #svm boundary





