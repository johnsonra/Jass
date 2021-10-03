library(keras)
library(magrittr)
library(purrr)
library(abind)
library(dplyr)
library(readr)
library(stringr)

# get root directory of the repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)
images <- '/cards'

# get all images in <images>
img_paths <- system(paste0('tree -if ', root, images, ' | grep jpg'), intern = TRUE) %>%
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
anno <- tibble(path = img_paths,
               info = str_replace(path, paste0(root, images, '/'), '') %>%
                      str_replace(fixed('.jpg'), '') %>%
                      str_split(fixed('_')),
               date = map(info, ~ as.Date(.x[1])) %>%
                      as.vector(),
               suit = map_chr(info, ~ substr(.x[3], 1, 1)),
               rank = map_chr(info, ~ substr(.x[3], 2, 2)))

anno_suit <- cbind(anno$suit == 'B', 
                   anno$suit == 'F',
                   anno$suit == 'S',
                   anno$suit == 'A')
colnames(anno_suit) <- c('Bells', 'Flowers', 'Shields', 'Acorns')

anno_rank <- cbind(anno$rank == '6',
                   anno$rank == '7',
                   anno$rank == '8',
                   anno$rank == '9',
                   anno$rank == 'B',
                   anno$rank == 'U',
                   anno$rank == 'O',
                   anno$rank == 'K',
                   anno$rank == 'A')
colnames(anno_rank) <- c('6', '7', '8', '9', 'B', 'U', 'O', 'K', 'A')
  
# check one prediction
application_vgg16(weights = 'imagenet') %>%
  predict(imgs[1,,,, drop = FALSE]) %>%
  imagenet_decode_predictions(top = 3)

# save processed data
save(imgs, anno, anno_suit, anno_rank, file = paste0(root, '/training/preprocess.RData'))
