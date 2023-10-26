# Install and load necessary packages if not already installed
if (!require("tm")) install.packages("tm", dependencies = TRUE)
if (!require("pdftools")) install.packages("pdftools", dependencies = TRUE)
library(tm)
library(pdftools)

# Define the path to your PDF file
pdf_file_path <- "/Users/leosponnich/Downloads/978-3-319-51020-0.pdf"

# Read the text from the PDF
pdf_text_data <- pdf_text(pdf_file_path)

# Preprocess the text
text <- tolower(paste(pdf_text_data, collapse = " "))
text <- removePunctuation(text)
text <- removeNumbers(text)
text <- stripWhitespace(text)

# Split the text into sentences (you can skip this step if summarizing based on full text)
sentences <- unlist(strsplit(text, "[.!?]"))

# Create a Corpus
corpus <- Corpus(VectorSource(sentences))

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Calculate TF-IDF scores
tfidf <- weightTfIdf(dtm)

# Calculate sentence scores
sentence_scores <- rowSums(as.matrix(tfidf))

# Identify the top N sentences for your summary
num_sentences_to_include <- 5
top_sentences <- order(sentence_scores, decreasing = TRUE)[1:num_sentences_to_include]

# Generate the summary
summary <- sentences[top_sentences]

# Display the summary
cat(paste(summary, collapse = " "))
