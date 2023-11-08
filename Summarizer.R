library(tidymodels)
library(tm)
library(quanteda)
load("fittedmodel.RData")

preprocess_text <- function(text) {
  #text <- tolower(text)
  text <- tokens(text, what = "sentence")
  text <- tokens_tolower(text)
  text <- tokens_remove(text, stopwords("english"))
  text <- tokens_replace(text, pattern = "\\p{P}", replacement = "")
  return(text)
}

# Code for choosing file here
input_path <- file.choose()


# Make summary


input_article = readLines(input_path)
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

ideallength <- log( length ( input_flattened ) ) + 2
ratio <- ideallength/length(input_flattened)


input_threshold <- quantile(input_prediction$.pred_1, 1-ratio)
top_30_percent <- input_prediction %>%
  filter(.pred_1 >= input_threshold)

top_30_indices <- which(input_prediction$.pred_1 >= input_threshold)

output <- input_flattened[top_30_indices]
output2 <- paste(output, collapse=" ")
print(output2)
