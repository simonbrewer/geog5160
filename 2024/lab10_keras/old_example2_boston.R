set.seed(1234)
## Example 2: boston house prices
library(dplyr)
library(keras)

dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)

str(test_data)

mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

## Set up model construction as a function (for k-fold)
build_model <- function() {                                1
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
}

num_epochs <- 100

model <- build_model()

model %>% fit(X_train, y_train,
              epochs = num_epochs, batch_size = 1)

## What about predictive power

num_epochs <- 100

k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
all_scores <- c()

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X_train[val_indices,]
  val_targets <- y_train[val_indices]
  partial_train_data <- X_train[-val_indices,] 
  partial_train_targets <- y_train[-val_indices]
  
  model <- build_model()
  
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results["mae"])
}

all_scores
mean(all_scores)

## 500 epochs with saved MAE (do this first)
num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()

ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()

model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 80, batch_size = 16)
result <- model %>% evaluate(test_data, test_targets)
result
