#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and classify photos via Convolutional Neural Network
## Date: 2020-03-28
## Author: Rahul Sharma
#######################################################################
library(keras)
library(EBImage)

# Read Images
train <- list()
for(i in 1:700) train[[i]] <- readImage(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\",pm$name[i]))

test <- list()
for(i in 1:100) test[[i]] <- readImage(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\",pm$name[(i+700)]))
  
# Resize & combine
for (i in 1:700) {train[[i]] <- resize(train[[i]], 100, 100)}
for (i in 1:100) {test[[i]] <- resize(test[[i]], 100, 100)}
train <- combine(train)
test <- combine(test)

# Reorder dimension
train <- aperm(train, c(4, 1, 2, 3))
test <- aperm(test, c(4, 1, 2, 3))

# Response
trainy <- as.numeric(pm$category == "outdoor-day")[1:700]
testy <- as.numeric(pm$category == "outdoor-day")[701:800]
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)

# Model
model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 16, 
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(100, 100, 3)) %>%
  layer_conv_2d(filters = 16,
      kernel_size = c(3,3),
      activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate=0.25) %>%
  layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = loss_categorical_crossentropy,
    optimizer = optimizer_adadelta(),
    metrics = c('accuracy')
  )

batch_size <- 64
epochs <- 32

# Train model
model %>% fit(
  train, trainLabels,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.2
)

# Evaluation & Prediction - train data
model %>% evaluate(train, trainLabels)
pred <- model %>% predict_classes(train)
table(Predicted = pred, Actual = trainy)
