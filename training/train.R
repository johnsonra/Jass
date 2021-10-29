# train.R
# main training script for Jass

library(tensorflow)
library(keras)
library(magrittr)
library(purrr)
library(abind)
library(dplyr)
library(readr)
library(stringr)

# directory paths
root <- system('git rev-parse --show-toplevel', intern = TRUE)
raw_images <- '/cards'
image_gen <- '/training/data/'


# process annotation data
paste0(root, '/training/preprocess.R') %>%
  source()


# train hand recognition model
paste0(root, '/training/card_recognition.R') %>%
  source()
