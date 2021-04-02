## Example 3: Beijing air pollution
set.seed(1234)
library(reticulate)
library(dplyr)
library(tidyr)
library(keras)

## Read data
dat <- read.csv("../datafiles/pollution.csv")

## Convert wnd_dir to numeric
dat$wnd_dir <- as.numeric(factor(dat$wnd_dir, levels = c("NE", "NW", "SE", "cv")))

normalize <- function(x) {
  x <- c(x)
  (x - min(x)) / (max(x) - min(x))
}

scaled <- dat %>%
  mutate(pollution = normalize(pollution),
         dew = normalize(dew),
         temp = normalize(temp),
         press = normalize(press),
         wnd_dir = normalize(wnd_dir),
         wnd_spd = normalize(wnd_spd),
         snow = normalize(snow),
         rain = normalize(rain))

pollution_min <- min(dat$pollution)
pollution_max <- max(dat$pollution)

lagged <- scaled %>%
  mutate(lag_1_pollution = lag(pollution, 1),
         lag_1_dew = lag(dew, 1),
         lag_1_temp = lag(temp, 1),
         lag_1_press = lag(press, 1),
         lag_1_wnd_dir = lag(wnd_dir, 1),
         lag_1_wnd_spd = lag(wnd_spd, 1),
         lag_1_snow = lag(snow, 1),
         lag_1_rain = lag(rain, 1)
  ) %>%
  drop_na() %>%
  select(pollution, contains("lag_1"))
  
n_obs = 365 * 24 ## SHould be -3

X_train <- data.matrix(lagged[1:n_obs, -1])
X_train <- array(X_train, dim = c(n_obs, 1, 8))
y_train <- lagged$pollution[1:n_obs]

X_test <- data.matrix(lagged[n_obs:nrow(lagged), -1])
X_test <- array(X_test, dim = c(nrow(X_test), 1, 8))
y_test <- lagged$pollution[n_obs:nrow(lagged)]

## ------------------------------------------------------------------------- ##
## Dense
# initialize our model
model <- keras_model_sequential()

# our input layer
model %>%
  layer_dense(input_shape = dim(X_train)[2:3], units = 50)

model %>%
  layer_dense(units = 1) # output

# look at our model architecture
summary(model)

## Compile it
model %>% compile(
  optimizer = optimizer_adam(), 
  loss = "mse", 
  metrics = c("mae")
)

# Actually train our model! This step will take a while
history <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = 64, # how many samples to pass to our model at a time
  epochs = 50, # how many times we'll look @ the whole dataset
  validation_data = list(X_test, y_test),
  shuffle = FALSE) # 

## -------------------------------------------------------------------------------------------
# Evaluate the model on the validation data
results <- model %>% keras::evaluate(X_test, y_test, verbose = 1)
results

back_transform <- function(x, xmin = 0, xmax = 1) {
  return((x * (xmax - xmin)) + xmin)
}

y_test_pred_norm <- model %>% predict(X_test)
y_test_pred <- back_transform(y_test_pred_norm, pollution_min, pollution_max)
y_obs <- back_transform(y_test, pollution_min, pollution_max)

mean(abs(y_test_pred - y_obs))

## ------------------------------------------------------------------------- ##
## LSTM
# initialize our model
model <- keras_model_sequential()

# our input layer
model %>%
  layer_lstm(input_shape = dim(X_train)[2:3], units = 50)

model %>%
  layer_dense(units = 1) # output

# look at our model architecture
summary(model)

## Compile it
model %>% compile(
  optimizer = optimizer_adam(), 
  loss = "mse", 
  metrics = c("mae")
)

# Actually train our model! This step will take a while
history <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = 64, # how many samples to pass to our model at a time
  epochs = 50, # how many times we'll look @ the whole dataset
  validation_data = list(X_test, y_test),
  shuffle = FALSE) # 

plot(history)
## -------------------------------------------------------------------------------------------
# Evaluate the model on the validation data
results <- model %>% keras::evaluate(X_test, y_test, verbose = 1)
results

y_test_pred_norm <- model %>% predict(X_test)
y_test_pred <- back_transform(y_test_pred_norm, pollution_min, pollution_max)
y_obs <- back_transform(y_test, pollution_min, pollution_max)

mean(abs(y_test_pred - y_obs))

## -------------------------------------------------------------------------------------------
# Three hour lag
lagged <- scaled %>%
  mutate(lag_1_pollution = lag(pollution, 1),
         lag_1_dew = lag(dew, 1),
         lag_1_temp = lag(temp, 1),
         lag_1_press = lag(press, 1),
         lag_1_wnd_dir = lag(wnd_dir, 1),
         lag_1_wnd_spd = lag(wnd_spd, 1),
         lag_1_snow = lag(snow, 1),
         lag_1_rain = lag(rain, 1)
  ) %>%
  mutate(lag_2_pollution = lag(pollution, 2),
         lag_2_dew = lag(dew, 2),
         lag_2_temp = lag(temp, 2),
         lag_2_press = lag(press, 2),
         lag_2_wnd_dir = lag(wnd_dir, 2),
         lag_2_wnd_spd = lag(wnd_spd, 2),
         lag_2_snow = lag(snow, 2),
         lag_2_rain = lag(rain, 2)
  ) %>%
  mutate(lag_3_pollution = lag(pollution, 3),
         lag_3_dew = lag(dew, 3),
         lag_3_temp = lag(temp, 3),
         lag_3_press = lag(press, 3),
         lag_3_wnd_dir = lag(wnd_dir, 3),
         lag_3_wnd_spd = lag(wnd_spd, 3),
         lag_3_snow = lag(snow, 3),
         lag_3_rain = lag(rain, 3)
  ) %>%
  drop_na() %>%
  select(pollution, contains("lag_1"), contains("lag_2"), contains("lag_3"))

n_obs = 365 * 24 ## SHould be -3

X_train <- data.matrix(lagged[1:n_obs, -1])
X_train <- array(X_train, dim = c(n_obs, 1, 8))
y_train <- lagged$pollution[1:n_obs]

X_test <- data.matrix(lagged[n_obs:nrow(lagged), -1])
X_test <- array(X_test, dim = c(nrow(X_test), 1, 8))
y_test <- lagged$pollution[n_obs:nrow(lagged)]

## LSTM
# initialize our model
model <- keras_model_sequential()

# our input layer
model %>%
  layer_lstm(input_shape = dim(X_train)[2:3], units = 50)

model %>%
  layer_dense(units = 1) # output

# look at our model architecture
summary(model)

## Compile it
model %>% compile(
  optimizer = optimizer_adam(), 
  loss = "mse", 
  metrics = c("mae")
)

# Actually train our model! This step will take a while
history <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = 64, # how many samples to pass to our model at a time
  epochs = 50, # how many times we'll look @ the whole dataset
  validation_data = list(X_test, y_test),
  shuffle = FALSE) # 

plot(history)

# Evaluate the model on the validation data
results <- model %>% keras::evaluate(X_test, y_test, verbose = 1)
results

y_test_pred_norm <- model %>% predict(X_test)
y_test_pred <- back_transform(y_test_pred_norm, pollution_min, pollution_max)
y_obs <- back_transform(y_test, pollution_min, pollution_max)

mean(abs(y_test_pred - y_obs))

stop()
## -------------------------------------------------------------------------------------------
## More complex structure
model <- keras_model_sequential()

model %>%
  layer_lstm(input_shape = dim(X_train)[2:3], 
             units = 100, 
             return_sequences = TRUE)  %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50, 
             return_sequences = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)


model %>%
  compile(loss = 'mae', optimizer = 'adam')

model

## Compile it
model %>% compile(
  optimizer = optimizer_adam(), 
  loss = "mse", 
  metrics = c("mae")
)

# Actually train our model! This step will take a while
history <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = 64, # how many samples to pass to our model at a time
  epochs = 20, # how many times we'll look @ the whole dataset
  validation_data = list(X_test, y_test),
  shuffle = FALSE) # 

plot(history)

# Evaluate the model on the validation data
results <- model %>% keras::evaluate(X_test, y_test, verbose = 1)
results

y_test_pred_norm <- model %>% predict(X_test)
y_test_pred <- back_transform(y_test_pred_norm, pollution_min, pollution_max)
y_obs <- back_transform(y_test, pollution_min, pollution_max)

mean(abs(y_test_pred - y_obs))
