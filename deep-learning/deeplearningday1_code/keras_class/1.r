#===============================================================================
# Model 1: MNIST Data Set
#
# Deep Learning Workshop Day 1
# Bundesbank, March 28th 2019
# 
# Chris Arnold, Cardiff University, March 2019
#===============================================================================


# Housekeeping ------------------------------------------------------------
library(keras)

# Data  -------------------------------------------------------------------
mnist <- dataset_mnist()

# train data
train_images <- mnist$train$x
str(train_images)
train_images <- array_reshape(train_images, c(60000, 28*28))
str(train_images)
train_images <- train_images/255

# test data 
test_images <- mnist$test$x
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images/255
str(train_labels)

# targets
train_labels <- mnist$train$y
test_labels <- mnist$test$y
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)



# Network Architecture ----------------------------------------------------

network <- keras_model_sequential() %>% 
    layer_dense(units = 512, activation = "relu", input_shape = c(28*28)) %>%
    layer_dense(units = 10, activation = "softmax")


# Compile -----------------------------------------------------------------

network %>% compile(
    optimizer = 'rmsprop',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
)


network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)






