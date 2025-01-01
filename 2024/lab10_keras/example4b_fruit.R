Sys.setenv(RETICULATE_PYTHON = "C:/Users/brewe/Documents/tf2/.venv/Scripts/python.exe")

# load libraries
library(tidyverse)
library(tensorflow)
library(keras)

tf$random$set_seed(42)

tf_version()

is_keras_available()

train_image_files_path <- "./fruits/Training/"

# list of fruits to model
fruit_list <- c("Kiwi", "Banana", "Apricot", "Avocado", "Cocos", "Clementine", "Mandarine", "Orange",
                "Limes", "Lemon", "Peach", "Plum", "Raspberry", "Strawberry", "Pineapple", "Pomegranate")

# number of output classes (i.e. fruits)
output_n <- length(fruit_list)

# image size to scale down to (original images are 100 x 100 px)
img_width <- 20
img_height <- 20
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# define batch size
batch_size <- 32

train_data_gen <- image_data_generator(
  rescale = 1/255,
  validation_split = 0.3)

# training images
train_image_array_gen <- flow_images_from_directory(train_image_files_path, 
                                                    train_data_gen,
                                                    subset = 'training',
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    batch_size = batch_size,
                                                    seed = 42)

# validation images
valid_image_array_gen <- flow_images_from_directory(train_image_files_path, 
                                                    train_data_gen,
                                                    subset = 'validation',
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    batch_size = batch_size,
                                                    seed = 42)

cat("Number of images per class:")
table(factor(train_image_array_gen$classes))

# Note, that even though Keras’ image_data_generator recognizes folder names as 
# class labels for classification tasks, these labels will be converted into indices 
# (alphabetical starting from 0) for training and prediction later. 
# Thus, it is useful to create a library object that matches these indices 
# back to human-interpretable labels.

train_image_array_gen_t <- train_image_array_gen$class_indices %>%
  as_tibble()


# number of training samples
train_samples <- train_image_array_gen$n
# number of validation samples
valid_samples <- valid_image_array_gen$n

# define number of epochs
epochs <- 10

# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", 
                input_shape = c(img_width, img_height, channels)) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>% 
  layer_activation("softmax")

# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

# fit
hist <- model %>% fit_generator(
  # training data
  train_image_array_gen,
  
  # epochs
  steps_per_epoch = as.integer(train_samples / batch_size), 
  epochs = epochs, 
  
  # validation data
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(valid_samples / batch_size)
)

## Predicting for new set of images
# path to image folders
test_image_files_path <- "./fruits/Test/"

test_data_gen <- image_data_generator(rescale = 1/255)

test_generator <- flow_images_from_directory(
  test_image_files_path,
  test_data_gen,
  target_size = target_size,
  class_mode = "categorical",
  classes = fruit_list,
  batch_size = 1,
  shuffle = FALSE,
  seed = 42)


model %>%
  evaluate_generator(test_generator, 
                     steps = as.integer(test_generator$n))

## Get predictions
classes <- test_generator$classes %>%
  factor() %>%
  table() %>%
  as.tibble()
colnames(classes)[1] <- "value"

indices <- test_generator$class_indices %>%
  as.data.frame() %>%
  gather() %>%
  mutate(value = as.character(value)) %>%
  left_join(classes, by = "value")

# predict on test data
test_generator$reset()
predictions <- model %>% 
  predict_generator(
    generator = test_generator,
    steps = as.integer(test_generator$n)
  ) %>%
  round(digits = 2) %>%
  as.tibble()

colnames(predictions) <- indices$key

predictions <- predictions %>%
  mutate(truth_idx = as.character(test_generator$classes)) %>%
  left_join(indices, by = c("truth_idx" = "value"))

pred_analysis <- predictions %>%
  mutate(img_id = seq(1:test_generator$n)) %>%
  gather(pred_lbl, y, Kiwi:Pomegranate) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key, n, pred_lbl) %>%
  count()

p <- pred_analysis %>%
  mutate(percentage_pred = nn / n * 100) %>%
  ggplot(aes(x = key, y = pred_lbl, 
             fill = percentage_pred,
             label = round(percentage_pred, 2))) +
  geom_tile() +
  scale_fill_continuous() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(color = "white") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "True class", 
       y = "Predicted class",
       fill = "Percentage\nof predictions",
       title = "True v. predicted class labels", 
       subtitle = "Percentage of test images predicted for each label")
p
