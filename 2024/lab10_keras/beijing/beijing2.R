## ----message = FALSE------------------------------------------------------------------------
set.seed(1234)
library(reticulate)
library(dplyr)
library(skimr)
library(caret)
library(keras)
library(tibble)
library(recipes)
library(readr)
library(ggpubr)

nlags <- 3

dat <- read.csv("data/pollution.csv")

# values <- dat[, -1]

## Convert wnd_dir to numeric
dat$wnd_dir <- as.numeric(factor(dat$wnd_dir, levels = c("NE", "NW", "SE", "cv")))

## -------------------------------------------------------------------------------------------
f1 <- pollution ~ dew + temp + press + wnd_dir + wnd_spd + snow + rain

model_recipe <- recipe(f1, 
                       data = dat)

# define the steps we want to apply
model_recipe_steps <- model_recipe %>% 
  # mean impute numeric variables
  # step_meanimpute(all_numeric()) %>%
  # convert the additional ingredients variable to dummy variables
  step_dummy(all_nominal()) %>%
  # rescale all numeric variables except for test rate
  # step_range(all_numeric(), min = 0, max = 1, -ltest_rate) %>%
  step_range(all_numeric(), min = 0, max = 1) %>%
  step_lag(all_predictors(), lag = c(1, 2, 3))
  # remove predictor variables that are almost the same for every entry
  #step_nzv(all_predictors()) 

prepped_recipe <- prep(model_recipe_steps, training = dat)
prepped_recipe

scaled <- bake(prepped_recipe, dat) 

head(scaled)
head(as.data.frame(scaled))

## Remove NA lags
scaled <- scaled[-c(1:3), ]
n_obs = 365 * 24 ## SHould be -3

X_train <- data.matrix(scaled[1:n_obs, 9:ncol(scaled)])
X_train <- array(X_train, dim = c(n_obs, nlags, 8))
y_train <- scaled$pollution[1:n_obs]

X_test <- data.matrix(scaled[n_obs:nrow(scaled), 9:ncol(scaled)])
X_test <- array(X_test, dim = c(nrow(X_test), nlags, 8))
y_test <- scaled$pollution[n_obs:nrow(scaled)]

stop()
# initialize our model
model <- keras_model_sequential()

# our input layer
# model %>%
#   layer_dense(input_shape = dim(X_train)[2:3], units = 5)

# model %>% 
#   layer_simple_rnn(units = 6)
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
  batch_size = 72, # how many samples to pass to our model at a time
  epochs = 50, # how many times we'll look @ the whole dataset
  validation_data = list(X_test, y_test),
  shuffle = FALSE) # 

plot(history)
## -------------------------------------------------------------------------------------------
# Evaluate the model on the validation data
results <- model %>% keras::evaluate(X_test, y_test, verbose = 1)
results

pred_test = model %>% predict(X_test)
print(postResample(pred = pred_test, obs = y_test))

