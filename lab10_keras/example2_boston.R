set.seed(1234)
## Example 2: boston house prices
library(dplyr)
library(keras)

boston <- read.csv("../datafiles/boston6k.csv")

X <- boston %>%
  select(CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT) %>%
  as.matrix()

y <- boston$CMEDV

train_set = sample(nrow(X), 0.8 * nrow(X))
test_set = setdiff(seq_len(nrow(X)), train_set)

X_train <- X[train_set, ]
X_test <- X[-train_set, ]

y_train <- y[train_set]
y_test <- y[-train_set]

mean <- apply(X_train, 2, mean)
std <- apply(X_train, 2, sd)
X_train <- scale(X_train, center = mean, scale = std)
X_test <- scale(X_test, center = mean, scale = std)

## Set up model construction as a function (for k-fold)
build_model <- function() {                                1
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(X_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
}

## k-fold x-validation
k <- 4
indices <- sample(1:nrow(X_train))
folds <- cut(indices, breaks = k, labels = FALSE)

## 500 epochs with saved MAE (do this first)
num_epochs <- 250
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  X_val <- X_train[val_indices,]
  
  
  y_val <- y_train[val_indices]
  
  X_partial_train <- X_train[-val_indices,]
  y_partial_train <- y_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    X_partial_train, y_partial_train,
    validation_data = list(X_val, y_val),
    epochs = num_epochs, batch_size = 16, verbose = 0
  )
  mae_history <- history$metrics$val_mae
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
model %>% fit(X_train, y_train,
              epochs = 80, batch_size = 16)
result <- model %>% evaluate(X_test, y_test)
result


