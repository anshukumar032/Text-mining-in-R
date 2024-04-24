
getwd()
setwd("D:/Project")
getwd()


install.packages("mongolite")


library(mongolite)

#----------------------Loading the data set-------------------------------------

connection_string <- 'mongodb+srv://anshu182003:tCgzYTpXC5TZh674@airbnb.9exsji6.mongodb.net/'
airbnb_collection <- mongo(collection="airbnb", db="sample_airbnb", url=connection_string)


airbnb_all <- airbnb_collection$find()


airbnb_all_flat <- jsonlite::flatten(airbnb_all)


list_cols <- sapply(airbnb_all_flat, function(x) is.list(x))


names(list_cols[list_cols == TRUE])



airbnb_all_flat$reviews <- NULL

airbnb_all_flat$address.location.coordinates <- NULL

airbnb_all_flat$amenities <- sapply(airbnb_all_flat$amenities, toString)

airbnb_all_flat$host.host_verifications <- sapply(airbnb_all_flat$host.host_verifications, toString)

write.csv(airbnb_all_flat, "airbnb_data.csv", row.names = FALSE)


#---------------- text mining--------------------------------------------


data <- read.csv("D:/Project/sample_airbnb_text.csv")

View(data)

colnames(data)

install.packages("tm")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("ggplot")
install.packages("syuzhet")
install.packages("tm")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("ggplot")
install.packages("syuzhet")
install.packages("ggplot2")
install.packages("SnowballC")
install.packages("proxy")
install.packages("igraph")
install.packages("tm.corpus")
install.packages("e1071")
install.packages("topicmodels") 
install.packages("sentimentr")


library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library("syuzhet")
library(dplyr)
library(ggplot)
library(stringr)
library(topicmodels)
library(igraph)
library(proxy)
library(tm)
library(SnowballC)
library(ggplot2)
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(dplyr)
library(ggplot)
library(stringr)
library(tm.corpus)
library(e1071)



#-----------------Text mining for the review------------------------------

data = read.csv("D:/Project/sample_airbnb_text.csv")

colnames(data)

#----------------------------Text mining of summary column ---------------------------


summary_1 <- data$summary



data2 <- data.frame(summary_1)




# Check the structure of the dataset
str(data2)

# Display the first few rows of the dataset
head(data2)

# Summary statistics of the dataset
summary(data2)


# Number of observations
num_obs <- nrow(data2)
cat("Number of observations:", num_obs, "\n")

words <- unlist(str_split(tolower(data2$summary_1), "\\W+"))



word_freq <- table(words)
top_words <- head(sort(word_freq, decreasing = TRUE), 10)
print(top_words)


ggplot(data, aes(x = nchar(data2))) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Summary Lengths",
       x = "Length of Summary",
       y = "Frequency") +
  theme_minimal()


# Create a word cloud of the most frequent words
wordcloud(words = names(top_words), freq = top_words, scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE,
          min.freq = 1,
          max.words = 100,
          random.color = TRUE,
          rot.per = 0.35,
          main = "Word Cloud of Most Frequent Words")




#----------------------------Text mining of description column using naive bayes ---------------------------

# Load required libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

des_1 = data$description



# Clean and preprocess the descriptions

clean_descriptions <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  # Remove punctuation
  text <- removePunctuation(text)
  # Remove numbers
  text <- removeNumbers(text)
  # Remove extra white spaces
  text <- stripWhitespace(text)
  # Tokenization
  text <- wordTokenize(text)
  # Remove stopwords
  text <- text[!text %in% stopwords("en")]
  # Stemming
  text <- wordStem(text)
  # Combine words back into a single string
  text <- paste(text, collapse = " ")
  return(text)
}



# Apply cleaning function to descriptions
cleaned_data <- sapply(des_1, tm::wordTokenize)


# Define a custom cleaning function
clean_text <- function(text) {
  # Convert text to lowercase
  text <- tolower(text)
  # Remove special characters, punctuation, and numbers
  text <- gsub("[^a-zA-Z\\s]", "", text)
  # Remove extra white spaces
  text <- gsub("\\s+", " ", text)
  # Trim leading and trailing white spaces
  text <- trimws(text)
  return(text)
}

cleaned_data <- sapply(des_1, clean_text)

# Tokenization
tokenized_text <- strsplit(cleaned_data, "\\s+")


# Count word frequency
word_freq_2 <- table(unlist(tokenized_text))

# Load stopwords


stopwords <- stopwords::stopwords("en")


# Remove stopwords
word_freq_2 <- word_freq_2[!(names(word_freq) %in% stopwords)]





missing_values <- sum(is.na(word_freq_2))


# Remove missing values from word_freq_2
word_freq_2 <- word_freq_2[!is.na(word_freq_2)]


# Generate word cloud
if (missing_values > 0) {
  warning("There were missing values in word_freq_2. They have been removed.")
}

wordcloud(names(word_freq_2), freq = word_freq_2, max.words = 100, random.order = FALSE)


# 1. Bar plot of word frequencies
barplot(word_freq_2[1:20], las = 2, col = "skyblue", main = "Top 20 Words Frequency", xlab = "Word", ylab = "Frequency")

# 2. Histogram of word lengths
word_lengths <- nchar(names(word_freq_2))
hist(word_lengths, col = "lightgreen", main = "Distribution of Word Lengths", xlab = "Word Length", ylab = "Frequency")




#---------------- Text mining to space Column using TF-IDF--------------------------------#
df<- read.csv("D:/Project/sample_airbnb_text.csv")

colnames(df)

spa_1 <- df$space

# Convert text data to a corpus
corpus <- Corpus(VectorSource(spa_1))


# Function to clean and preprocess text
# Convert text to lowercase
text <- tolower(text){
  # Remove punctuation
  text <- removePunctuation(text)
  # Remove numbers
  text <- removeNumbers(text)
  # Remove stopwords
  text <- removeWords(text, stopwords("english"))
  # Stemming
  text <- wordStem(text)
  # Strip whitespace
  text <- stripWhitespace(text)
  return(text)
}


# Apply cleaning function to the corpus
cleaned_corpus <- tm_map(corpus, content_transformer(clean_text))

# Create Document-Term Matrix using TF-IDF
dtm <- DocumentTermMatrix(cleaned_corpus, control = list(weighting = function(x) weightTfIdf(x)))

# Convert Document-Term Matrix to a matrix
dtm_matrix <- as.matrix(dtm)

# Convert matrix to data frame
dtm_df <- as.data.frame(as.matrix(dtm))

# Add row names (document names)
rownames(dtm_df) <- paste0("Document", 1:nrow(dtm_df))

# Calculate total word frequencies
total_word_freq <- colSums(dtm_matrix)

# Sort the terms by frequency
sorted_terms <- sort(total_word_freq, decreasing = TRUE)

# Select top 10 terms
top_terms <- head(sorted_terms, 10)

# Create a bar plot for top terms
barplot(top_terms, main = "Top 10 Most Frequent Terms", xlab = "Terms", ylab = "Frequency")

# Calculate document frequencies
doc_freq <- colSums(dtm_matrix > 0)


# Create a histogram for document frequencies
hist(doc_freq, main = "Histogram of Document Frequencies", xlab = "Document Frequencies", ylab = "Number of Terms")



# Word Cloud
word_freq_3 <- sort(colSums(dtm_matrix), decreasing = TRUE)
wordcloud(names(word_freq_3), word_freq_3, max.words = 100, colors = brewer.pal(8, "Dark2"))




#---------Text mining of reviews[1].comments Column using SENTIMENT --------------------#

reviews <- df$reviews.1..comments

reviews_df <- data.frame(comments = reviews)

bing_lexicon <- get_sentiments("bing")


sentiment_analysis <- reviews_df %>%
  unnest_tokens(word, clean_comments) %>%
  inner_join(bing_lexicon) %>%
  count(sentiment) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) # Fo



# Create a bar plot of sentiment scores
sentiment_plot <- ggplot(sentiment_analysis, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Sentiment Analysis of Reviews",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()

# Display the plot
print(sentiment_plot)


sentiment_plot <- ggplot(sentiment_analysis, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3) + # Add data labels
  labs(title = "Sentiment Analysis of Reviews",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()

print(sentiment_plot)


sentiment_pie <- ggplot(sentiment_analysis, aes(x = "", y = n, fill = sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Sentiments",
       fill = "Sentiment",
       x = NULL, y = NULL) +
  theme_void()

# Display the pie chart
print(sentiment_pie)


sentiment_analysis <- sentiment_analysis %>%
  mutate(percentage = n / sum(n) * 100)

# Create a pie chart to visualize the distribution of sentiments
sentiment_pie <- ggplot(sentiment_analysis, aes(x = "", y = n, fill = sentiment, 
                                                label = paste0(sentiment, ": ", round(percentage, 1), "%"))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +  # Add data labels
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Sentiments",
       fill = "Sentiment",
       x = NULL, y = NULL) +
  theme_void()



# Display the pie chart
print(sentiment_pie)










