#===============================================================================
# Model 4
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


# Data --------------------------------------------------------------------
dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)
str(test_data)
str(train_targets)


# Preparing the Data ------------------------------------------------------
# Normalise 
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)


# Model -------------------------------------------------------------------
build_model <- function() {
    model <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu",
                    input_shape = dim(train_data)[[2]]) %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_dense(units = 1)
    model %>% compile(
        optimizer = "rmsprop",
        loss = "mse",
        metrics = c("mae")
    ) }




# K-fold Validation -------------------------------------------------------
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 100
all_scores <- c()

for (i in 1:k) {
    cat("processing fold #", i, "\n")
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indices,]
    val_targets <- train_targets[val_indices]
    
    partial_train_data <- train_data[-val_indices,]
    partial_train_targets <- train_targets[-val_indices]
    model <- build_model()
    
    model %>% fit(partial_train_data, partial_train_targets,
                  epochs = num_epochs, batch_size = 1, verbose = 0)
    
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_scores <- c(all_scores, results$mean_absolute_error)
}

all_scores
mean(all_scores)




# Saving the Logs each fold -----------------------------------------------

num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
    cat("processing fold #", i, "\n")
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indices,]
    val_targets <- train_targets[val_indices]
    partial_train_data <- train_data[-val_indices,]
    partial_train_targets <- train_targets[-val_indices]
    model <- build_model()
    history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = num_epochs, batch_size = 1, verbose = 0
    )
    mae_history <- history$metrics$val_mean_absolute_error
    all_mae_histories <- rbind(all_mae_histories, mae_history)
}


# Successive validation scores --------------------------------------------

average_mae_history <- data.frame(
    epoch = seq(1:ncol(all_mae_histories)),
    validation_mae = apply(all_mae_histories, 2, mean)
)


# Plot --------------------------------------------------------------------

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()

ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()



# Train the best model ----------------------------------------------------

model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 80, batch_size = 16, verbose = 0)
result <- model %>% evaluate(test_data, test_targets)
result


