# process.R - called from train.R


set.seed(39824)
# get all images in <images> and parse file names
imgs <- tibble(path = paste0('ls ', root, raw_images) %>%
                      system(intern = TRUE),
               info = str_replace(path, fixed('.jpg'), '') %>%
                      str_split(fixed('_')),
               date = map(info, ~ as.Date(.x[1])) %>%
                      as.vector(),
               set  = map_chr(info, ~ .x[2]),
               card = map_chr(info, ~ .x[3])) %>%
  
  # pick train, validate and test sets
  group_by(card) %>%
  mutate(validate = 1:length(set) %in% sample( 1:length(set)            , size = 2),
         test     = 1:length(set) %in% sample((1:length(set))[!validate], size = 2),
         train    = !validate & !test) %>%
  ungroup() %>%
  
  mutate(subdir = case_when(train    ~ 'train/',
                            validate ~ 'validate/',
                            test     ~ 'test/',
                            TRUE     ~ ''))
                            

# process images for input (set up for generator)
for(i in 1:nrow(imgs))
{
  # check that the directory for this card exists - if not, create
  if(!dir.exists(paste0(root, image_gen, imgs$subdir[i], imgs$card[i])))
    dir.create(paste0(root, image_gen, imgs$subdir[i], imgs$card[i]))
  
  # import image, i
  img <- paste0(root, raw_images, '/', imgs$path[i]) %>%
    image_load(target_size = c(224, 224)) %>%
    image_to_array() %>%
    
    # preprocess image
    imagenet_preprocess_input() %>%
    
    # convert to jpg
    tf$image$encode_jpeg() %>%
  
    # write processed image ####################### this bit needs work... ################################
    tf$write_file(paste0(root, image_gen, imgs$subdir[i], imgs$card[i], '/', imgs$card[i], imgs$set[i], '.jpg'))
}

# imgs <- sapply(img_paths, image_load, target_size = c(224, 224)) %>%  # read images
#   lapply(image_to_array) %>%                                          # convert to arrays
#   map(~ array_reshape(.x, c(1, dim(.x)))) %>%                         # reshape such that the first dimension is the sample
#   abind(along = 1) %>%                                                # merge list into a single array
#   imagenet_preprocess_input()
# 
# #imgs <- imgs / 255                             # normalize to [0,1] - don't need this for imagenet, I think
# 
# # format annotations
# anno <- tibble(path = img_paths,
#                info = str_replace(path, paste0(root, images, '/'), '') %>%
#                       str_replace(fixed('.jpg'), '') %>%
#                       str_split(fixed('_')),
#                date = map(info, ~ as.Date(.x[1])) %>%
#                       as.vector(),
#                suit = map_chr(info, ~ substr(.x[3], 1, 1)),
#                rank = map_chr(info, ~ substr(.x[3], 2, 2)))
# 
# anno_suit <- cbind(anno$suit == 'B', 
#                    anno$suit == 'F',
#                    anno$suit == 'S',
#                    anno$suit == 'A')
# colnames(anno_suit) <- c('Bells', 'Flowers', 'Shields', 'Acorns')
# 
# anno_rank <- cbind(anno$rank == '6',
#                    anno$rank == '7',
#                    anno$rank == '8',
#                    anno$rank == '9',
#                    anno$rank == 'B',
#                    anno$rank == 'U',
#                    anno$rank == 'O',
#                    anno$rank == 'K',
#                    anno$rank == 'A')
# colnames(anno_rank) <- c('6', '7', '8', '9', 'B', 'U', 'O', 'K', 'A')
#   
# # check one prediction
# application_vgg16(weights = 'imagenet') %>%
#   predict(imgs[1,,,, drop = FALSE]) %>%
#   imagenet_decode_predictions(top = 3)
# 
# # save processed data
# save(imgs, anno, anno_suit, anno_rank, file = paste0(root, '/training/preprocess.RData'))
