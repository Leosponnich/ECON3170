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
categories <- c("business", "politics", "sport", "tech")
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

# Add sentence length and sentence position as features
sentence_lengths <- sapply(preprocessed_articles, function(article) sapply(article, nchar))
unlisted_articles <- unlist(preprocessed_articles)
#sentence_positions <- as.list(seq_along(unlisted_articles))
#sentence_positions2 <- lapply(unlisted_articles, function(x) #find position here)

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
  #Article = unlist(preprocessed_articles),
  #SentenceLength = unlist(sentence_lengths),
  SentenceLength = unlisted_lengths,
  #SentencePosition = sentence_positions[1],
  AverageTFIDF = unlist(sentence_scores),
  BelongsInSummary = unlist(article_belongsinsummary)
)

# Add the target variable
#preprocessed_data$BelongsInSummary <- as.numeric(preprocessed_data$Article %in% preprocessed_data$Summary)

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

# Make predictions
predictions <- predict(fit, new_data = data_test, type="prob")



input_path <- file.path(root_dir, "News Articles", "entertainment", "001.txt")
input_article = readLines(articlePath)
input_cleaned <- Filter(nzchar, input_article)
input_preprocessed = preprocess_text(input_cleaned)

input_sentence_lengths <- sapply(input_preprocessed, nchar)

input_corpus <- Corpus(VectorSource(input_preprocessed))
input_dtm <- DocumentTermMatrix(input_corpus)
input_tfidf <- weightTfIdf(input_dtm)
input_tfidf_scores <- as.data.frame(as.matrix(input_tfidf))
input_sentence_avg_scores <- rowMeans(input_tfidf_scores)

input_flattened <- unlist(input_preprocessed)

input_preprocessed_data <- data.frame(
  SentenceLength = unlist(input_sentence_lengths),
  AverageTFIDF = unlist(input_sentence_avg_scores)
  #BelongsInSummary = unlist(article_belongsinsummary)
)

input_prediction <- predict(fit, new_data = input_preprocessed_data, type="prob")

input_threshold <- quantile(input_prediction$.pred_1, 0.6)
top_30_percent <- input_prediction %>%
  filter(.pred_1 >= input_threshold)

top_30_indices <- which(input_prediction$.pred_1 >= input_threshold)

output <- input_flattened[top_30_indices]
output2 <- paste(output, collapse=" ")
print(output2)
