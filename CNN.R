#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and classify photos via Convolutional Neural Network
## Date: 2020-03-28
## Author: Rahul Sharma
#######################################################################

library(keras)
library(EBImage)

# Read metadata
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
y <- as.numeric(pm$category == "outdoor-day")

# Read Images
pics <- list()
for (i in 1:800){
  pics[[i]] <- readImage(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\", pm$name[i]))
  pics[[i]] <- resize(pics[[i]], 256, 256)
}

train_index <- sample(1:length(pics), 0.8 * length(pics))
test_index <- setdiff(1:length(pics), train_index)


x.train <- pics[c(train_index)]
y.train <- pics[-c(train_index)]

x.test <- pics[c(test_index)]
y.test <- pics[-c(test_index)]

x.train <- combine(x.train)
x.test <- combine(x.test)

# Reorder dimension
x.train <- aperm(x.train, c(4, 1, 2, 3))
x.test <- aperm(x.test, c(4, 1, 2, 3))

# Response
cat.train <- to_categorical(as.numeric(pm$category == "outdoor-day")[c(train_index)])
cat.test <- to_categorical(as.numeric(pm$category == "outdoor-day")[c(test_index)])

#Model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(128,128,3)) %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>% 
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 10,
              activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes,
              activation = 'softmax')

model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

batch_size <- 64
epochs <- 16

# Train model
model %>% fit(
  x.train,
  cat.train,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.2
)

score <- model %>% evaluate(x.test,cat.test)

cat('Test loss: ', score$loss, "\n")

cat('Test accuracy: ', score$acc, "\n")
