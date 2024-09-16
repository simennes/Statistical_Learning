library(ISLR)
library(GGally)
data(Auto)
head(Auto)

printInfo <- function(dataset){
  for(i in 1:7){
    print(colnames(dataset)[i])
    print(c(range(dataset[,i]), mean(dataset[,i]), sqrt(var(dataset[,i]))))
    print("-----------------------", quote=FALSE)
  }
}

printInfo(Auto)

ReducedAuto <- Auto[10:85,]
printInfo(ReducedAuto)

ggpairs(Auto[,1:7])


ggplot(Auto, aes(x=origin, y=mpg)) +
  geom_boxplot(fill = "skyblue", aes(group=origin)) +
  labs(title = "Box Plot") +
  theme_minimal()

ggplot(Auto, aes(x=cylinders, y=mpg)) +
  geom_boxplot(fill = "skyblue", aes(group=cylinders)) +
  labs(title = "Box Plot") +
  theme_minimal()

###########################

library(MASS)
mu <- c(2,3)
sigma1 <- matrix(c(1,0,0,1), ncol=2)
sigma2 <- matrix(c(1,0,0,5), ncol=2)
sigma3 <- matrix(c(1,2,2,5), ncol=2)
sigma4 <- matrix(c(1,-2,-2,5), ncol=2)

obs1 <- mvrnorm(1000, mu, sigma1)
obs2 <- mvrnorm(1000, mu, sigma2)
obs3 <- mvrnorm(1000, mu, sigma3)
obs4 <- mvrnorm(1000, mu, sigma4)

df <- data.frame(obs1,obs2,obs3,obs4)
df

ggplot(df) +
  geom_point(aes(obs1[,1], obs1[,2], color = "obs1")) +
  geom_point(aes(obs2[,1], obs2[,2], color = "obs2")) +
  geom_point(aes(obs3[,1], obs3[,2], color = "obs3")) +
  geom_point(aes(obs4[,1], obs4[,2], color = "obs4")) +
  scale_color_manual(values = c("pink", "lightblue", "lightgreen", "orange"))


##########################
set.seed(2) # to reproduce
M <- 100 # repeated samplings, x fixed
nord <- 20 # order of polynomials
#------
x <- seq(from = -2, to = 4, by = 0.1)
truefunc <- function(x) {
  return(sin(x))
}
true_y <- truefunc(x)
error <- matrix(rnorm(length(x) * M, mean = 0, sd = 0.5),
                nrow = M,
                byrow = TRUE)
ymat <- matrix(rep(true_y, M), byrow = T, nrow = M) + error  # Each row is a simulation
#------
predictions_list <- lapply(1:nord, matrix, data = NA, nrow = M, ncol = ncol(ymat))
for(i in 1:nord){
  for(j in 1:M){
    predictions_list[[i]][j, ] <- predict(lm(ymat[j,] ~ poly(x, i, raw = TRUE)))
  }
}
# Plotting -----
library(tidyverse) # The tidyverse contains ggplot2, as well as tidyr and dplyr, 
# which we can use for dataframe manipulation.
list_of_matrices_with_deg_id <- 
  lapply(1:nord, 
         function(poly_degree){cbind(predictions_list[[poly_degree]], 
                                     simulation_num = 1:M, poly_degree)}
  )
# Now predictions_list is a list with 20 entries, where each entry is a matrix 
# with 100 rows, where each row is the predicted polynomial of that degree.
# We also have a column for the simulation number, and a column for polynomial degree.
# Extract each matrix and bind them to one large matrix
stacked_matrices <-  NULL
for (i in 1:nord) {
  stacked_matrices <-
    rbind(stacked_matrices, list_of_matrices_with_deg_id[[i]])
}
stacked_matrices_df <- as.data.frame(stacked_matrices)
# Convert from wide to long (because that is the best format for ggplot2)
long_predictions_df <- pivot_longer(stacked_matrices_df, 
                                    !c(simulation_num, poly_degree), 
                                    values_to = "y")
# Now we can use ggplot2!
# We just want to plot for degrees 1, 2, 10 and 20.
plotting_df <- cbind(long_predictions_df, x = x) %>% # adding the x-vector to the dataframe 
  filter(poly_degree %in% c(1, 3, 10, 20)) # Select only the predictions using degree 1, 2, 10 or 20
ggplot(plotting_df, aes(x = x, y = y, group = simulation_num)) +
  geom_line(aes(color = simulation_num)) +
  geom_line(aes(x = x, y = sin(x)), size = 1.5) +
  facet_wrap(~ poly_degree) +
  theme_bw() +
  theme(legend.position = "none")

########################################

set.seed(2) # to reproduce
M <- 100 # repeated samplings,x fixed but new errors
nord <- 20
x <- seq(from = -2, to = 4, by = 0.1)
truefunc <- function(x){
  return(sin(x))
}

true_y <- truefunc(x)
error <- matrix(rnorm(length(x)*M, mean = 0, sd = 0.5), nrow = M, byrow = TRUE)
testerror <- matrix(rnorm(length(x)*M, mean = 0, sd = 0.5), nrow = M, byrow = TRUE)
ymat <- matrix(rep(true_y, M), byrow = T, nrow = M) + error
testymat <- matrix(rep(true_y, M), byrow=T, nrow=M) + testerror
predictions_list <- lapply(1:nord, matrix, data = NA, nrow = M, ncol = ncol(ymat))
for(i in 1:nord){
  for(j in 1:M){
    predictions_list[[i]][j, ] <- predict(lm(ymat[j,] ~ poly(x, i, raw = TRUE)))
  }
}
trainMSE <- lapply(1:nord, 
                   function(poly_degree){
                     rowMeans((predictions_list[[poly_degree]] - ymat)^2)}
)
testMSE <- lapply(1:nord, 
                  function(poly_degree){
                    rowMeans((predictions_list[[poly_degree]] - testymat)^2)}
)

library(tidyverse) # The tidyverse contains ggplot2, as well as tidyr and dplyr, 
# which we can use for dataframe manipulation.
# Convert each matrix in the list form wide to long (because that is the best format for ggplot2)
list_train_MSE <- lapply(1:nord, function(poly_degree) cbind(error = trainMSE[[poly_degree]], 
                                                             poly_degree, 
                                                             error_type = "train",
                                                             simulation_num = 1:M))
list_test_MSE <- lapply(1:nord, function(poly_degree) cbind(error = testMSE[[poly_degree]], 
                                                            poly_degree, 
                                                            error_type = "test", 
                                                            simulation_num = 1:M))
# Now predictions_list is a list with 20 entries, where each entry is a matrix 
# with 100 rows, where each row is the predicted polynomial of that degree.
stacked_train <- NULL
for (i in 1:nord) {
  stacked_train <-
    rbind(stacked_train, list_train_MSE[[i]])
}
stacked_test <- NULL
for (i in 1:nord) {
  stacked_test <-
    rbind(stacked_test, list_test_MSE[[i]])
}
stacked_errors_df <- as.data.frame(rbind(stacked_train, stacked_test))
# This is already on long format. 
stacked_errors_df$error <- as.numeric(stacked_errors_df$error)
stacked_errors_df$simulation_num <- as.integer(stacked_errors_df$simulation_num)
stacked_errors_df$poly_degree <- as.integer(stacked_errors_df$poly_degree)
p.all_lines <- ggplot(data = stacked_errors_df, 
                      aes(x = poly_degree, y = error, group = simulation_num)) +
  geom_line(aes(color = simulation_num)) +
  facet_wrap(~ error_type) +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw() +
  theme(legend.position = "none")
p.bars <- ggplot(stacked_errors_df, aes(x = as.factor(poly_degree), y = error)) +
  geom_boxplot(aes(fill = error_type)) +
  scale_fill_discrete(name = "Error type") +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw()
# Here we find the average test error and training error across the repeated simulations. 
# The symbol "%>%" is called a pipe, and comes from the tidyverse packages, 
# which provide convenient functions for working with data frames.
means_across_simulations <- stacked_errors_df %>% 
  group_by(error_type, poly_degree) %>% 
  summarise(mean = mean(error))
p.means <- ggplot(means_across_simulations, aes(x = poly_degree, y = mean)) +
  geom_line(aes(color = error_type)) +
  scale_color_discrete(name = "Error type") +
  xlab("Polynomial degree") +
  ylab("MSE") +
  theme_bw()
library(patchwork) # The library patchwork is the best way of combining ggplot2 objects. 
# You could also use the function ggarrange from the ggpubr package.
p.all_lines / (p.bars + p.means)


###########################
meanmat <- matrix(ncol = length(x), nrow = nord)
varmat <- matrix(ncol = length(x), nrow = nord)
for (j in 1:nord){
  meanmat[j,] <- apply(predictions_list[[j]], 2, mean) # we now take the mean over the M simulations - to mimic E and Var at each x value and each poly model
  varmat[j,] <- apply(predictions_list[[j]], 2, var)
}

# nord times length(x)
bias2mat <- (meanmat - matrix(rep(true_y, nord), byrow = TRUE, nrow = nord))^2 #here the truth is finally used!

df <- data.frame(x = rep(x, each = nord), poly_degree = rep(1:nord, length(x)), 
                 bias2 = c(bias2mat), variance = c(varmat), 
                 irreducible_error = rep(3^2, prod(dim(varmat)))) #irr is just 1

df$total <- df$bias2 + df$variance + df$irreducible_error

df_long <- pivot_longer(df, cols = !c(x, poly_degree), names_to = "type") 

df_select_poly <- filter(df_long, poly_degree %in% c(2, 3, 4, 5))

ggplot(df_select_poly, aes(x = x, y = value, group = type)) +
  geom_line(aes(color = type)) +
  facet_wrap(~poly_degree, scales = "free", labeller = label_both) +
  theme_bw()