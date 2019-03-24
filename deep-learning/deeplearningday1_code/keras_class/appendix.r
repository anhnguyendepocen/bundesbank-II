#===============================================================================
# Useful Code Snippets
# 
# Deep Learning Workshop Day 1
# Bundesbank, March 28th 2019
# 
# Chris Arnold, Cardiff University, March 2019
#===============================================================================



# magrittr package --------------------------------------------------------

http://r4ds.had.co.uk/pipes.html



# Model 2: Reviews --------------------------------------------------------

word_index <- dataset_imdb_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index
decoded_review <- sapply(train_data[[1]], function(index) {
    word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
    if (!is.null(word)) word else "?"
})
decoded_review
