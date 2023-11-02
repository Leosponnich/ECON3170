library(tm)
library(quanteda)

#Read data
# Define the root directory where the data is located
root_dir <- file.path(getwd(), "BBC News Summary")

# Create a list to store articles and summaries
articles <- list()
summaries <- list()

# Loop through the categories and read the data
categories <- c("business", "entertainment","politics","sport","tech")
for (category in categories) {
  article_dir <- file.path(root_dir, "news articles", category)
  summary_dir <- file.path(root_dir, "summaries", category)
  
  article_files <- list.files(article_dir, full.names = TRUE)
  summary_files <- list.files(summary_dir, full.names = TRUE)
  
  # Read and store articles and summaries
  articles <- c(articles, lapply(article_files, readLines))
  summaries <- c(summaries, lapply(summary_files, readLines, warn = FALSE))
  
  
}
cleaned_articles <- lapply(articles, function(article) Filter(nzchar, article))
cleaned_summaries <- lapply(summaries, function(summary) Filter(nzchar, summary))



#preprocess text
# Example text preprocessing using quanteda

#unlist(strsplit(string, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))

preprocess_text <- function(text) {
  #text <- tolower(text)
  text <- tokens(text, what = "sentence")
  text <- tokens_tolower(text)
  text <- tokens_remove(text, stopwords("english"))
  text <- tokens_replace(text, pattern = "\\p{P}", replacement = "")
  return(text)
  
}

# Preprocess articles and summaries
preprocessed_articles <- lapply(articles, preprocess_text)

preprocessed_summaries <- lapply(summaries, preprocess_text)

# Filter out inner lists with no text
filtered_preprocessed_articles <- preprocessed_articles[sapply(preprocessed_articles, function(x) length(unlist(x)) > 0)]

# If you want to also remove the corresponding original articles, you can do the same:
filtered_articles <- articles[sapply(preprocessed_articles, function(x) length(unlist(x)) > 0)]


#allScores = list()

#for (article in preprocessed_articles) {

  article <- preprocessed_articles[[1]]
  
  for (line in article) {
    
    
    
  }

  corpus <- Corpus(VectorSource(article))
  
  dtm <- DocumentTermMatrix(corpus)
  tfidf <- weightTfIdf(dtm)
  tfidf_scores <- as.data.frame(as.matrix(tfidf))
  
  allScores <- append(allScores, tfidf_scores)
  
#}



#organize data
# Create data frames or lists to store the preprocessed data
# For example, you can create a data frame with columns for category, article, and summary.
preprocessed_data <- data.frame(
  Article = preprocessed_articles,
  Summary = preprocessed_summaries
)

# You can also store the preprocessed data in a list for easier access.
preprocessed_data_list <- list(
  tech = preprocessed_data[preprocessed_data$Category == "tech", ],
  Business = preprocessed_data[preprocessed_data$Category == "Business", ]
)



#split in to training, validation and test datasets
# Set the random seed for reproducibility
set.seed(123)

# Define the proportion of data for each split
train_proportion <- 0.7
validation_proportion <- 0.15
test_proportion <- 0.15

# Initialize variables to store the data splits
train_data <- list()
validation_data <- list()
test_data <- list()

# Loop through each category (e.g., 'tech' and 'Business')
categories <- names(preprocessed_data_list)

for (category in categories) {
  data_category <- preprocessed_data_list[[category]]
  
  # Split the data based on the proportions
  num_samples <- nrow(data_category)
  num_train <- round(train_proportion * num_samples)
  num_validation <- round(validation_proportion * num_samples)
  num_test <- num_samples - num_train - num_validation
  
  # Shuffle the data
  shuffled_data <- data_category[sample(1:num_samples), ]
  
  # Extract the splits
  train_data[[category]] <- shuffled_data[1:num_train, ]
  validation_data[[category]] <- shuffled_data[(num_train + 1):(num_train + num_validation), ]
  test_data[[category]] <- shuffled_data[(num_train + num_validation + 1):(num_train + num_validation + num_test), ]
}

# Combine the splits for the final data sets
train_data_final <- do.call(rbind, train_data)
validation_data_final <- do.call(rbind, validation_data)
test_data_final <- do.call(rbind, test_data)
