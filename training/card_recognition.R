# card_recognition.R

# set up environment
root <- system('git rev-parse --show-toplevel', intern = TRUE)

paste0(root, '/training/preprocess.RData') %>%
  load()

# load imagenet weights (see https://cran.r-project.org/web/packages/keras/vignettes/applications.html)
base_model <- application_vgg16(weights = 'imagenet', include_top = FALSE)

# add a couple of layers on top
predictions <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 1024, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'softmax')

# model to train
model <- keras_model(inputs = base_model$input, outputs = predictions)

# freeze base layers from imagenet
freeze_weights(base_model)

# compile model
model %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy')

# train the model for a bit??
validation_split <- 0.9

set.seed(283746)
train <- rbinom(nrow(imgs), size = 1, prob = validation_split) %>% as.logical()

model %>% fit(imgs[train,,,], as.matrix(as.integer(anno_suit[train,])),
              validation_data = list(imgs[!train,,,], as.matrix(as.integer(anno[!train,]))))
