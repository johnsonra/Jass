# train.R
# master training script for Jass


# get root directory of the repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)


# process annotation data
if(!file.exists(paste0(root, '/training/preprocess.RData')))
{
  paste0(root, '/training/preprocess.R') %>%
    source()
}else{
  load(paste0(root, '/training/preprocess.RData'))
}


# train hand recognition model
paste0(root, '/training/hand_recognition.R') %>%
  source()
