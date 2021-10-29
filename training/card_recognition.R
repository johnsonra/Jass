# card_recognition.R - called from train.R


#########
# Model #
#########

# load imagenet weights (see https://cran.r-project.org/web/packages/keras/vignettes/applications.html)
base_model <- application_vgg16(weights = 'imagenet', include_top = FALSE)

# add a couple of layers on top
predictions <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 1024, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 36, activation = 'softmax')

# model to train
model <- keras_model(inputs = base_model$input, outputs = predictions)

# freeze base layers from imagenet
freeze_weights(base_model)


########
# Data #
########

train_datagen <- image_data_generator(
  rotation_range = 180,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

train_generator <- flow_images_from_directory(
  paste0(root, image_gen, 'train'),            # Target directory  
  train_datagen,                               # Data generator
  target_size = c(224, 224),
  batch_size = 20,
  class_mode = "categorical"
)

validation_generator <- flow_images_from_directory(
  paste0(root, image_gen, 'validation'),
  image_data_generator(),
  target_size = c(224, 224),
  batch_size = 20,
  class_mode = "categorical"
)


#########
# Train #
#########

# compile model
model %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('accuracy'))

# train the model for a bit
model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)
