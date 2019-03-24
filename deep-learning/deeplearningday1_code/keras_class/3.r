#===============================================================================
# Model 3: IMDB Data Set
#
# Deep Learning Workshop Day 1
# Bundesbank, March 28th 2019
# 
# Chris Arnold, Cardiff University, March 2019
#===============================================================================
# Goal:
# Systematic Investigation of Hyper Parameters
# Learning rate α
# iterations
# hidden layers l
# hidden units n[1], n[2], ...
# choice of activation function

# Housekeeping ------------------------------------------------------------
library(keras)


# Data  -------------------------------------------------------------------
imdb <- dataset_imdb(num_words = 10000)
train_data <- imdb$train$x
train_labels <- imdb$train$y
test_data <- imdb$test$x
test_labels <- imdb$test$y
str(train_data[[1]]) # sequence of words
train_labels[[1]]   # 1 is positive, 0 negative


# Data transformation -----------------------------------------------------
# Turning list of int into tensors
# 1 hot encoding: e.g. turn [3,5] into vector with length 10 000 and 0 -- except 
# for position 3 and 5 where it takes a 1
vectorize_sequences <- function(sequences, dimension = 10000) {
    results <- matrix(0, nrow = length(sequences), ncol = dimension)
    for (i in 1:length(sequences))
        results[i, sequences[[i]]] <- 1
    results
}
x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)
str(x_train[1,])
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)






# Model -------------------------------------------------------------------
network <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

# Why not using Stochastic Gradient descent and defining a learning rate?
sgd <- optimizer_sgd(lr = 0.01)
network %>% compile(
    optimizer = sgd,
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

# Run 
network %>% fit(x_train, y_train, epochs = 5, batch_size = 128)

# predict the likelihood that a review is positive
network %>% predict(x_test[1:10,])









# Test or Validation Set --------------------------------------------------


val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]


history <- network %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

str(history)
plot(history)











# Regularisation --------------------------------------------------------
# What do we do about overfitting? 

network_regul <- keras_model_sequential() %>%
    layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001),
                activation = "relu", input_shape = c(10000)) %>%
    layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001),
                activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

# Compile
network_regul %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

history <- network_regul %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)


