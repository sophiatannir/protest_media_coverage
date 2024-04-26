library(tidyverse)

sentiment <- read_csv('/Users/sophiatannir/Documents/Vanderbilt_DSI/Spring2024/capstone/results/sentiment_results.csv')

stories <- sentiment %>%
  select(row2, text2) %>%
  unique()

# Load necessary libraries
library(topicmodels)
library(tidytext)
library(tm)

text_corpus <- Corpus(VectorSource(stories$text2))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, removeWords, c("abstract", "headline", "lead paragraph", "protests", "protesters"))

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(text_corpus)

k_values <- seq(2, 30, by = 1)  # Adjust this range based on your needs
perplexities <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  lda_model <- LDA(dtm, k = k_values[i], control = list(seed = 1234))
  perplexities[i] <- perplexity(lda_model, dtm)
}

# Create a data frame for plotting
plot_data <- data.frame(K = k_values, Perplexity = perplexities)

# Create the elbow plot
ggplot(plot_data, aes(x = K, y = Perplexity)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Elbow Plot for Optimal Number of Topics",
       x = "Number of Topics",
       y = "Perplexity")

# Fit LDA model
k <- 10  # You can change the number of topics based on your analysis
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# View topics
topics <- terms(lda_model, 6)  # Retrieves top 6 terms per topic
print(topics)

# Assign topics to documents
topics_per_document <- topics(lda_model)
doc_topics <- tidy(lda_model, matrix = "gamma")
doc_topics <- doc_topics %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup()

# View document topics
print(doc_topics)

theta <- as.data.frame(lda_model@gamma)
rownames(theta) <- all_scores$story_index

combined_data <- cbind(theta, all_scores)

weighted_mean_for_topic <- function(topic_column, score_column) {
  sum(topic_column * score_column, na.rm = TRUE) / sum(topic_column, na.rm = TRUE)
}

number_of_topics <- ncol(theta)
topic_averages <- data.frame(Topic = integer(), Avg_Sentiment = numeric(), Avg_Paradigm = numeric())

for (i in 1:number_of_topics) {
  avg_sentiment <- weighted_mean_for_topic(combined_data[[i]], combined_data$scores_sentiment)
  avg_paradigm <- weighted_mean_for_topic(combined_data[[i]], combined_data$scores_paradigm)
  topic_averages <- rbind(topic_averages, 
                          data.frame(Topic = i, Dimension = "Sentiment", Avg_Score = avg_sentiment, Labels = topic_labels[i]),
                          data.frame(Topic = i, Dimension = "Paradigm", Avg_Score = avg_paradigm, Labels = topic_labels[i]))
}

print(topic_averages)

top_terms <- terms(lda_model, 3)
topic_labels <- apply(top_terms, 2, paste, collapse = ", ")

sentiment_plot <- ggplot(subset(topic_averages, Dimension == "Sentiment"), 
                         aes(x = reorder(Labels, Avg_Score), y = Avg_Score, fill = Dimension)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Sentiment Scores by Topic",
       x = "Topics (Top 3 Words)",
       y = "Average Sentiment Score") +
  theme(legend.position = "none")

sentiment_plot

paradigm_plot <- ggplot(subset(topic_averages, Dimension == "Paradigm"), 
                        aes(x = reorder(Labels, Avg_Score), y = Avg_Score, fill = Dimension)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Paradigm Scores by Topic",
       x = "Topics (Top 3 Words)",
       y = "Average Paradigm Score") +
  theme(legend.position = "none")

paradigm_plot

paradigm_rankings <- topic_averages %>%
  filter(Dimension == "Paradigm") %>%
  arrange(desc(Avg_Score)) %>%
  mutate(Ranking = row_number()) %>%
  select(Topic, Ranking)

topic_averages <- topic_averages %>%
  left_join(paradigm_rankings, by = "Topic")

topic_averages <- topic_averages %>%
  arrange(Ranking)

topic_averages$Labels <- factor(topic_averages$Labels, levels = unique(topic_averages$Labels))


ggplot(topic_averages, aes(x = Labels, y = Avg_Score, fill = Dimension)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~ Dimension, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Sentiment" = "steelblue", "Paradigm" = "darkorange")) +
  labs(title = "Average Scores by Topic Ordered by Paradigm",
       x = "Topics (Top 3 Words)",
       y = "Average Score") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "none")
