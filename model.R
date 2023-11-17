# Load the necessary libraries
install.packages('tidymodels')
install.packages('quanteda')
install.packages('tm')

library(tidymodels)
library(tm)
library(quanteda)

# Read data
# Define the root directory where the data is located
root_dir <- file.path(getwd(), "BBC News Summary")

# Create a list to store articles and summaries
articles <- list()
summaries <- list()

# Loop through the categories and read the data
categories <- c("business", "entertainment", "politics", "sport", "tech")
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

preprocess_text <- function(text) {
  #text <- tolower(text)
  text <- tokens(text, what = "sentence")
  text <- tokens_tolower(text)
  text <- tokens_remove(text, stopwords("english"))
  text <- tokens_replace(text, pattern = "\\p{P}", replacement = "")
  return(text)
}

# Preprocess articles and summaries
preprocessed_articles <- lapply(cleaned_articles, preprocess_text)
preprocessed_summaries <- lapply(cleaned_summaries, preprocess_text)

# Add sentence length as feature
sentence_lengths <- sapply(preprocessed_articles, function(article) sapply(article, nchar))
unlisted_articles <- unlist(preprocessed_articles)

# Calculate TF-IDF scores for each sentence
sentence_scores <- lapply(preprocessed_articles, function(article) {
  corpus <- Corpus(VectorSource(article))
  dtm <- DocumentTermMatrix(corpus)
  tfidf <- weightTfIdf(dtm)
  tfidf_scores <- as.data.frame(as.matrix(tfidf))
  sentence_avg_scores <- rowMeans(tfidf_scores)
  return(sentence_avg_scores)
})

unlisted_lengths = unlist(sentence_lengths)

flattened_articles <- lapply(preprocessed_articles, function(article) {
  unlist(article)
})

article_belongsinsummary <- lapply(seq_along(flattened_articles), function(articleindex) {
  lapply(seq_along(flattened_articles[[articleindex]]), function(sentenceindex) {
    sentence <- flattened_articles[[articleindex]][[sentenceindex]]
    summary <- preprocessed_summaries[[articleindex]]
    
    presentInSummary <- as.numeric(any(grepl(sentence, summary, fixed = TRUE)))
    return(presentInSummary)
  })
})

# Create the preprocessed_data data frame
preprocessed_data <- data.frame(
  SentenceLength = unlisted_lengths,
  AverageTFIDF = unlist(sentence_scores),
  BelongsInSummary = unlist(article_belongsinsummary)
)

# Split the data into training and testing sets
data_split <- initial_split(preprocessed_data, prop = 0.8, strata = "BelongsInSummary")
data_train <- training(data_split)
data_test <- testing(data_split)

# Define the model
model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Define the workflow
workflow <- workflow() %>%
  add_model(model) %>%
  add_recipe(recipe(BelongsInSummary ~ ., data = data_train))

# Convert BelongsInSummary to factor
data_train$BelongsInSummary <- factor(data_train$BelongsInSummary)

# Fit the model
fit <- fit(workflow, data = data_train)
