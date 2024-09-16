library(ISLR)
Auto <- subset(Auto, select = -name)
Auto$origin <- factor(Auto$origin)
summary(Auto)
str(Auto)

library(GGally)
#ggpairs(Auto)
cor(subset(Auto, select = -origin))

r.mpg <- lm(mpg ~ cylinders + displacement + horsepower 
            + weight + acceleration + year + origin,
            data = Auto)
summary(r.mpg)

# Comments:

#i) Clear relation between year and mpg, also weight and mpg
#ii) Looking at beta_weight. If the car weight increases by 1000kg we
#    would expect the mpg to DEcrease by approx 6.8 on average
#iii) That years have a big influence on mpg. If year increases by 10, we
#     we would expect mpg to INcrease by approx 7.5

library(ggplot2)
library(ggfortify)
autoplot(r.mpg, smooth.colour = NA)

# Comments
# Residuals vs fitted gives hint of possible nonlinear relationship
# Would also check observation 14 because of levrage


set.seed(2332)
n <- 100

par(mfrow = c(2, 3))
for (i in 1:6){
  sim <- rnorm(n)
  qqnorm(sim, pch = 1, frame = FALSE)
  qqline(sim, col = "blue", lwd = 1)
}

#-------------------

r.mpg2 <- lm(mpg ~ displacement + weight + year*origin,
            data = Auto)

summary(r.mpg2)
autoplot(r.mpg2, smooth.colour = NA)
anova(r.mpg2)

#Sqrt transform:

r.mpg2 <- lm(sqrt(mpg) ~ displacement + weight + year*origin,
             data = Auto)

summary(r.mpg2)
autoplot(r.mpg2, smooth.colour = NA)

#Squared transform:

r.mpg2 <- lm(mpg^2 ~ displacement + weight + year*origin,
             data = Auto)

summary(r.mpg2)
autoplot(r.mpg2, smooth.colour = NA)

#Logaritmic transform:

r.mpg2 <- lm(log(mpg) ~ displacement + weight + year*origin,
             data = Auto)

summary(r.mpg2)
autoplot(r.mpg2, smooth.colour = NA)


# CI interp.

beta0 <- -4.77
beta1 <- 2.14
true_beta <- c(beta0, beta1) # vector of model coefficients
true_sd <- 1 # choosing true sd
nobs <- 100
X <- runif(nobs, 0, 1) # simulate the predictor variable X
Xmat <- model.matrix(~X, data = data.frame(X)) # create design matrix

# Count how many times the true value is within the confidence interval
ci_int <- ci_x <- 0
nsim <- 1000
for (i in 1:nsim){
  y <- rnorm(n = nobs, mean = Xmat %*% true_beta, sd = rep(true_sd, nobs))
  mod <- lm(y ~ x, data = data.frame(y = y, x = X))
  ci <- confint(mod)
  
  # if true value of beta0 is within the CI then 1 else 0
  ci_int[i] <- ifelse(beta0 > ci[1,1] & beta0 < ci[1,2], 1, 0)
  
  # if true value of beta_1 is within the CI then 1 else 0
  ci_x[i] <- ifelse(beta1 > ci[2,1] & beta1 < ci[2,2], 1, 0)
}

c(mean(ci_int), mean(ci_x))

# PI interp.

beta0 <- -4.77
beta1 <- 2.14
true_beta <- c(beta0, beta1) # vector of model coefficients
true_sd <- 1 # choosing true sd
nobs <- 100
X <- runif(nobs, 0, 1) # simulate the predictor variable X
Xmat <- model.matrix(~X, data = data.frame(X)) # create design matrix
x0 <- 0.4
true_val <- beta0 + x0*beta1

# Count how many times the true value is within the confidence interval
pi_x <- 0
nsim <- 1000
for (i in 1:nsim){
  y <- rnorm(n = nobs, mean = Xmat %*% true_beta, sd = rep(true_sd, nobs))
  mod <- lm(y ~ x, data = data.frame(y = y, x = X))
  y0 <- rnorm(1,true_val,true_sd)
  pi <- predict(mod, newdata = data.frame(x = x0), interval = "predict")[2:3]
  
  # if true value of y0 is within the PI then 1 else 0
  pi_x[i] <- ifelse(y0 >= pi[1] && y0 <= pi[2], 1, 0)
  
}
mean(pi_x)