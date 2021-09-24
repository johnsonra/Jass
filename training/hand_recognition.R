# hand_recognition.R

# load imagenet weights (see https://cran.r-project.org/web/packages/keras/vignettes/applications.html)
base_model <- application_vgg16(weights = 'imagenet', include_top = FALSE)

# add a couple of layers on top
predictions <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 1024, activation = 'relu') %>%
  layer_dense(units = 36, activation = 'sigmoid')

# model to train
model <- keras_model(inputs = base_model$input, outputs = predictions)

# freeze base layers from imagenet
freeze_weights(base_model)

# compile model
model %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')

# train the model for a bit??
validation_split <- 0.9

set.seed(283746)
train <- rbinom(nrow(imgs), size = 1, prob = validation_split) %>% as.logical()

model %>% fit(imgs[train,,,], as.matrix(anno[train,-(1:3)]),
              validation_data = list(imgs[!train,,,], as.matrix(anno[!train,-(1:3)])))
