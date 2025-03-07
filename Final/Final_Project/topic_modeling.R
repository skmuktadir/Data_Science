# Load necessary libraries
library(rvest)
library(stringr)
library(tm)
library(tokenizers)
library(SnowballC)
library(textstem)
library(stringi)
library(hunspell)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)

# Step 1: Web Scraping
url <- "https://en.prothomalo.com/international/south-asia/s93mpidt19"
webpage <- read_html(url)

text_data <- webpage %>%
  html_nodes("p") %>%
  html_text()

# Step 2: Text Preprocessing
corpus <- Corpus(VectorSource(text_data))

# Custom stopwords list
custom_stopwords <- c(stopwords("en"), "said", "will", "also", "one", "can")

expand_contractions <- function(text) {
  text <- gsub("can't", "cannot", text)
  text <- gsub("won't", "will not", text)
  text <- gsub("'re", " are", text)
  text <- gsub("'ve", " have", text)
  text
}

remove_emojis <- function(text) {
  stri_replace_all_regex(text, "[\U0001F600-\U0001F64F]", "")
}

# Apply preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(expand_contractions))
corpus <- tm_map(corpus, content_transformer(remove_emojis))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))

# Step 3: Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.95)

# Step 4: LDA Topic Modeling
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Top 10 terms for each topic
topics <- terms(lda_model, 10)
print(topics)

# Topic Proportions
topic_proportions <- posterior(lda_model)$topics
print(topic_proportions)

# Step 5: Visualization of Topics (Enhanced Wordcloud)
for (i in 1:5) {
  # Open a new plot for each topic
  dev.new()
  
  # Extract the top 20 terms for the topic
  topic_terms <- terms(lda_model, 20)[, i]
  
  # Calculate actual term frequencies
  term_freq <- colSums(as.matrix(dtm)) 
  freq <- term_freq[topic_terms]
  
  # Handle any missing frequencies (NAs)
  freq[is.na(freq)] <- 1
  
  # Generate the word cloud
  wordcloud(words = topic_terms,
            freq = freq,
            scale = c(4, 0.5),         # Scale to avoid cutting words
            max.words = 20,
            random.order = FALSE,
            rot.per = 0.2,             # Slight word rotation
            colors = brewer.pal(8, "Dark2"),
            main = paste("Topic", i))
}
