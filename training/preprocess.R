library(keras)
library(magrittr)
library(purrr)
library(abind)
library(dplyr)
library(readr)

# get root directory of the repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)

# get all images in raw_hands/
img_paths <- system(paste0('tree -if ', root, '/raw_hands | grep jpg'), intern = TRUE) %>%
  gsub(pattern = '\033[01;35m', replacement = '', fixed = TRUE) %>%
  gsub(pattern = '\033[00m', replacement = '', fixed = TRUE)


# process images for input
imgs <- sapply(img_paths, image_load, target_size = c(224, 224)) %>%  # read images
  lapply(image_to_array) %>%                                          # convert to arrays
  map(~ array_reshape(.x, c(1, dim(.x)))) %>%                         # reshape such that the first dimension is the sample
  abind(along = 1) %>%                                                # merge list into a single array
  imagenet_preprocess_input()

#imgs <- imgs / 255                             # normalize to [0,1] - don't need this for imagenet, I think

# format annotations
anno <- read_csv(paste0(root, '/raw_hands/hands.csv')) %>%
  mutate(game = as.numeric(game)) %>%
  arrange(game, hand)

# check one prediction
application_vgg16(weights = 'imagenet') %>%
  predict(imgs[1,,,, drop = FALSE]) %>%
  imagenet_decode_predictions(top = 3)

# save processed data
save(imgs, anno, file = paste0(root, '/training/preprocess.RData'))
