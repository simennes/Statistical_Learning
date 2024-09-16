library(ISLR)
library(ggplot2)
library(ggfortify)
# extract only the two variables from Auto
ds = Auto[c("horsepower", "mpg")]
n = nrow(ds)
# which degrees we will look at
deg = 1:4
set.seed(1)
# training ids for training set
tr = sample.int(n, n/2)
# plot of training data
plot(ds[tr, ], col = "darkgrey", main = "Polynomial regression")

poly_mean_sqerr <- c()
for(i in 1:4){
  fit <- lm(mpg ~ poly(horsepower, i), data = ds[tr,])
  data <- data.frame(ds[tr,]$horsepower, fit$fitted.values)
  lines(data[order(data[,1]),],col=i,lwd=2)
  legend("topright", legend=paste("d =",1:4),col=1:4,lty=1,lwd=2)
  poly_mean_sqerr <- c(poly_mean_sqerr, mean((predict(fit,newdata = ds[-tr,])-ds[-tr,]$mpg)^2))
}
poly_mean_sqerr

#------------------------- 2 ------------------------

ds2 = Auto[c("origin", "mpg")]
fit2 <- lm(mpg~ factor(origin), data = ds2[tr,])

fit_frame <- data.frame(ds2[tr,]$origin,fit2$fitted.values)
names(fit_frame) <- c("origin", "fitted")
ses <- predict(fit2,newdata = ds2[-tr,],se=T)$se
ggplot(fit_frame, aes(origin,fitted)) +
  geom_point() +
  geom_segment(aes(x=origin, y=fitted-ses, xend = origin, yend=fitted+ses))


#----------------------- 4 --------------------------

# X_1
mybs = function(x, knots) {
  cbind(x, x^2, x^3, sapply(knots, function(y) pmax(0, x - y)^3))
}

d = function(c, cK, x) (pmax(0, x - c)^3 - pmax(0, x - cK)^3)/(cK - c)
# X_2
myns = function(x, knots) {
  kn = c(min(x), knots, max(x))
  K = length(kn)
  sub = d(kn[K - 1], kn[K], x)
  cbind(x, sapply(kn[1:(K - 2)], d, kn[K], x) - sub)
}
# X_3
myfactor = function(x) model.matrix(~x)[, -1]


#install.packages('gam')
library(gam)
library(ISLR)
# Figure out how to define your X-matrix (this is perhaps a bit tricky!)
X = cbind(rep(1,nrow(Wage)),mybs(Wage$age,c(40,60)),myns(Wage$year,c(2006)),myfactor(Wage$education))
# fitted model with our X
myhat = lm(wage ~ X - 1, data=Wage)$fit
# fitted model with gam
yhat = gam(wage ~ bs(age, knots = c(40, 60)) + ns(year, knots = 2006) + education,data=Wage)$fit
# are they equal?
all.equal(myhat, yhat)


#---------------------- 5 ------------------

Auto$origin <- as.factor(Auto$origin)
gamobject <- gam(mpg ~ bs(displacement, knots = c(290)) + poly(horsepower,2) +
                   weight + s(acceleration,df=3) + origin, data=Auto)

par(mfrow=c(2,3))
plot(gamobject,se=TRUE,col="blue")