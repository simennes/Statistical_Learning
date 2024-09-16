# ---------------- Problem 3 ---------------

library(dplyr)
library(MASS)
library(keras)
library(tensorflow)
library(ggplot2)
data(Boston)
library(caret)

# load
dataset <- Boston
tt.train <- sort(sample(1:506, 404, replace = FALSE))
x_train <- dataset[tt.train, 1:13]
y_train <- dataset[tt.train, 14]
x_test <- dataset[-tt.train, 1:13]
y_test <- dataset[-tt.train, 14]

# preprocess
mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)
x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)

# a)

# Define model
model_r <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = 13) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

summary(model_r)

# COpile model
model_r %>% compile(
  loss = "mean_squared_error",  # fill in the loss function.
  optimizer = optimizer_adam(learning_rate = 0.001),  # adam is the most common optimizer for its robustness.
  metrics = c("mean_absolute_error")
)

# Train model
history <- model_r %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_data = list(x_test, y_test)
)

# Store and print scores
scores <- model_r %>% evaluate(x_test, y_test, verbose = 0)

cat("Test loss (MSE):", scores[[1]], "\n",
    "Test mean absolute error (MAE):", scores[[2]], "\n")

plot(history)

# Store predictions and plot
predictions <- model_r %>% predict(x_test)
plot_df <- data.frame(Predicted = predictions, Actual = y_test)
ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  ggtitle("Predicted vs. Actual Values (Feedforward NN)") +
  xlim(0, 55) +
  ylim(0, 55)

# b)

# Fit a linear regression model
linear_model <- lm(y_train~., data = as.data.frame(cbind(x_train, y_train)))  # fill in the expression

# Make predictions on the test set
predictions <- predict(linear_model, as.data.frame(x_test))

# Calculate the mean squared error and mean absolute error
mse <- sum((predictions - y_test)^2)/length(predictions)
mae <- sum(abs(predictions - y_test))/length(predictions) 

cat("=== [Feedforward Neural Network] === \n", "Test loss (MSE):", scores[[1]], 
    "\n",
    "Test mean absolute error (MAE):", scores[[2]], 
    "\n",
    "==================================== \n\n",
    "=== [Linear Regression] === \n",
    "Test loss (MSE):", mse, "\n",
    "Test mean absolute error (MAE):", mae, "\n",
    "===========================\n\n")

plot_df <- data.frame(Predicted = predictions, Actual = y_test)
ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  ggtitle("Predicted vs. Actual Values (Linear Regression)") +
  xlim(0, 55) +
  ylim(0, 55)

# Very similar. Probably because linear relationship is the best
# for a bias-variance tradeoff. Any more variability in the
# nn would probably lead to higher test-error.


# ----------------- Oppgave 4 ----------------

cifar10 <- dataset_cifar10()
x_train <- cifar10$train$x / 255
y_train <- to_categorical(cifar10$train$y, num_classes = 10)
x_test <- cifar10$test$x / 255
y_test <- to_categorical(cifar10$test$y, num_classes = 10)


# a)

# Define model
model_c <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(32, 32, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

summary(model_c)

# Compile model
model_c %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.001),  # adam is the most common optimizer for its robustness.
  metrics = c("accuracy")
)

# Train model
history <- model_c %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 64,
  validation_data = list(x_test, y_test)
)

# Test
scores <- model_c %>% evaluate(x_test, y_test, verbose = 0)

cat("Test loss:", scores[[1]], "\n",
    "Test accuracy:", scores[[2]], "\n")

# Plot history
plot(history)


predictions <- model_c %>% predict(x_test)%>% k_argmax()
y_true <- cifar10$test$y
confusion_matrix <- confusionMatrix(factor(as.vector(predictions)), factor(y_true))
print(confusion_matrix$table)




# b)

# 1) Load and preprocess data
cifar10 <- dataset_cifar10()
x_train <- cifar10$train$x / 255
y_train <- to_categorical(cifar10$train$y, num_classes = 10)
x_test <- cifar10$test$x / 255
y_test <- to_categorical(cifar10$test$y, num_classes = 10)

# 2) Define the model
model_ca <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(32, 32, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# 3) Compile
model_ca %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("accuracy")
)

# 4) Data augmentation
datagen <- image_data_generator(
  rotation_range = 10,
  width_shift_range = 0.1,
  height_shift_range = 0.1,
  horizontal_flip = TRUE
)

# Compute the data generator internal statistics
datagen %>% fit_image_data_generator(x_train)

# 5) Train the model with data augmentation
batch_size = 64
train_generator <- flow_images_from_data(x = x_train, y = y_train, generator = datagen, batch_size = batch_size)

history <- model_ca %>% fit_generator(
  generator = train_generator,
  steps_per_epoch = as.integer(nrow(x_train) / batch_size),
  epochs = 10,
  validation_data = list(x_test, y_test)
)