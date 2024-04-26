library(tidyverse)
library(BradleyTerry2)

sentiment <- read_csv('/Users/sophiatannir/Documents/Vanderbilt_DSI/Spring2024/capstone/results/sentiment_results.csv')

sentiment <- sentiment %>%
  select(!...1)

bt_data_sentiment <- data.frame(
  player1 = as.factor(ifelse(sentiment$preference == 1, sentiment$row1, sentiment$row2)),
  player2 = as.factor(ifelse(sentiment$preference == 1, sentiment$row2, sentiment$row1)),
  outcome = 1
)

bt_data_sentiment$player1 <- relevel(bt_data_sentiment$player1, ref = 140)
bt_data_sentiment$player2 <- relevel(bt_data_sentiment$player2, ref = 140)

bt_model_sentiment <- BTm(player1 = bt_data_sentiment$player1, player2 = bt_data_sentiment$player2, outcome = bt_data_sentiment$outcome, data = bt_data_sentiment)



summary(bt_model_sentiment)

scores_sentiment <- coef(bt_model_sentiment)
print(scores_sentiment)

scores_sentiment_df <- as.data.frame(scores_sentiment)

scores_sentiment_df$story_index <- rownames(scores_sentiment_df)

quartiles_sentiment <- quantile(scores_sentiment, probs = c(0, 0.25, 0.5, 0.75, 1))
quartiles_sentiment



# PARADIGM RESULTS ----
paradigm <- read_csv("paradigm_results.csv")

paradigm <- paradigm %>%
  select(!...1)

bt_data_paradigm <- data.frame(
  player1 = as.factor(ifelse(paradigm$preference == 1, paradigm$row1, sentiment$row2)),
  player2 = as.factor(ifelse(paradigm$preference == 1, paradigm$row2, paradigm$row1)),
  outcome = 1
)

common_levels <- union(levels(bt_data_paradigm$player1), levels(bt_data_paradigm$player2))
bt_data_paradigm$player1 <- factor(bt_data_paradigm$player1, levels = common_levels)
bt_data_paradigm$player2 <- factor(bt_data_paradigm$player2, levels = common_levels)

bt_data_paradigm$player1 <- relevel(bt_data_paradigm$player1, ref = 140)
bt_data_paradigm$player2 <- relevel(bt_data_paradigm$player2, ref = 140)

bt_model_paradigm <- BTm(player1 = bt_data_paradigm$player1, player2 = bt_data_paradigm$player2, 
                         outcome = bt_data_paradigm$outcome, data = bt_data_paradigm)

summary(bt_model_paradigm)

scores_paradigm <- coef(bt_model_paradigm)
print(scores_paradigm)

plot(scores_paradigm, scores_sentiment)
cor(scores_paradigm, scores_sentiment)

scores_paradigm_df <- as.data.frame(scores_paradigm)

scores_paradigm_df$story_index <- rownames(scores_paradigm_df)

scores_paradigm_clean <- scores_paradigm_df %>%
  filter(scores_paradigm >= -10 & scores_paradigm <= 10)

all_scores <- inner_join(scores_paradigm_df, scores_sentiment_df)

all_scores_no_outliers <- inner_join(scores_sentiment_df, scores_paradigm_clean)

plot(all_scores_no_outliers$scores_sentiment, all_scores_no_outliers$scores_paradigm)
cor(all_scores$scores_sentiment, all_scores$scores_paradigm)

plot(all_scores$scores_sentiment, all_scores$scores_paradigm)

library(ggplot2)

ggplot(all_scores, aes(x = scores_sentiment, y = scores_paradigm)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  labs(x = "Sentiment Scores", y = "Paradigm Scores",
       title = "Relationship between Sentiment and Paradigm Scores") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  annotate("text", x = min(all_scores$scores_sentiment), y = max(all_scores$scores_paradigm),
           label = paste0("Correlation: ", round(cor(all_scores$scores_sentiment, all_scores$scores_paradigm), 2)),
           hjust = 0, vjust = 1, size = 4, color = "black")

reshaped_data <- bind_rows(
  sentiment %>%
    group_by(row1) %>%
    summarise(prob_chosen = mean(preference)) %>%
    mutate(dimension = "Sentiment"),
  
  paradigm %>%
    group_by(row1) %>%
    summarise(prob_chosen = mean(preference)) %>%
    mutate(dimension = "Paradigm")
)

ggplot(reshaped_data, aes(x = row1, y = prob_chosen)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ dimension, scales = "free_x", ncol = 2) +
  labs(x = "Article Index", y = "Probability of Being Chosen",
       title = "Probabilities of Articles Being Chosen as More Positive or Focused on Demands") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

ggplot(reshaped_data, aes(x = row1, y = prob_chosen, color = dimension)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Sentiment" = "steelblue", "Paradigm" = "darkorange")) +
  facet_wrap(~ dimension, scales = "free_x", ncol = 2) +
  labs(x = "Article Index", y = "Probability of Being Chosen",
       title = "Probabilities of Articles Being Chosen as More Positive or Focused on Demands") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "none")

ggplot(reshaped_data, aes(x = prob_chosen, fill = dimension)) +
  geom_histogram(binwidth = 0.1, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Sentiment" = "steelblue", "Paradigm" = "darkorange")) +
  facet_wrap(~ dimension, scales = "free_x", ncol = 2) +
  labs(x = "Probability of Being Chosen", y = "Frequency",
       title = "Distribution of Probabilities for Articles Being Chosen\nas More Positive or Focused on Demands") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "none")

ggplot(scores_sentiment_df, aes(x = scores_sentiment)) +
  geom_histogram(binwidth = 0.1, alpha = 0.7, fill = "steelblue") +
  labs(x = "Sentiment Scores", y = "Frequency",
       title = "Distribution of Sentiment Scores") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

ggplot(scores_paradigm_df, aes(x = scores_paradigm)) +
  geom_histogram(binwidth = 1, alpha = 0.7, fill = "darkorange") +
  labs(x = "Paradigm Scores", y = "Frequency",
       title = "Distribution of Paradigm Scores") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

