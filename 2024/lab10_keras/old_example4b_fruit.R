## Example 4: fruit image classification
## Courtesy: Dr. Shirin Glander. Code source: https://shirinsplayground.netlify.com/2018/06/keras_fruits/

library(keras)
start <- Sys.time()
fruit_list <- c("Kiwi", "Banana", "Plum", "Apricot", "Avocado", "Cocos", "Clementine", "Mandarine", "Orange",
                "Limes", "Lemon", "Peach", "Plum", "Raspberry", "Strawberry", "Pineapple", "Pomegranate")

# number of output classes (i.e. fruits)
output_n <- length(fruit_list)

# image size to scale down to (original images are 100 x 100 px)
img_width <- 20
img_height <- 20
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# path to image folders
path <- "./"
train_image_files_path <- file.path(path, "fruits-360", "Training")
valid_image_files_path <- file.path(path, "fruits-360", "Test")

# optional data augmentation
train_data_gen <- image_data_generator(
  rescale = 1/255
)

# Validation data shouldn't be augmented! But it should also be scaled.
valid_data_gen <- image_data_generator(
  rescale = 1/255
)  

# training images
train_image_array_gen <- flow_images_from_directory(train_image_files_path, 
                                                    train_data_gen,
                                                    target_size = target_size,
                                                    class_mode = 'categorical',
                                                    classes = fruit_list,
                                                    seed = 42)

# validation images
valid_image_array_gen <- flow_images_from_directory(valid_image_files_path, 
                                                    valid_data_gen,
                                                    target_size = target_size,
                                                    class_mode = 'categorical',
                                                    classes = fruit_list,
                                                    seed = 42)

table(factor(train_image_array_gen$classes))
train_image_array_gen$class_indices

fruits_classes_indices <- train_image_array_gen$class_indices
save(fruits_classes_indices, file = "./fruits-360/fruits_classes_indices.RData")

### model definition
# number of training samples
train_samples <- train_image_array_gen$n
# number of validation samples
valid_samples <- valid_image_array_gen$n

# define batch size and number of epochs
batch_size <- 32
epochs <- 10

# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
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
hist <- fit_generator(
  model,
  # training data
  train_image_array_gen,
  
  # epochs
  steps_per_epoch = as.integer(train_samples / batch_size), 
  epochs = epochs,
  
  # validation data
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(valid_samples / batch_size),
  
  # print progress
  verbose = 2,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint(file.path(path, "fruits_checkpoints.h5"), save_best_only = TRUE),
    # only needed for visualising with TensorBoard
    callback_tensorboard(log_dir = file.path(path, "fruits_logs"))
  )
)

df_out <- hist$metrics %>% 
  {data.frame(acc = .$acc[epochs], val_acc = .$val_acc[epochs], elapsed_time = as.integer(Sys.time()) - as.integer(start))}