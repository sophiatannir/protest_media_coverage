library(httr)
library(jsonlite)
library(dplyr)
library(openai)

# API key removed for privacy

# Query parameters
query <- "protests"
current_date <- Sys.Date()
start_year <- as.numeric(format(current_date, "%Y")) - 2
start_date <- as.Date(paste(start_year, format(current_date, "%m-%d"), sep="-"))

# Corrected date range for the past 2 years
end_date <- Sys.Date()

articles_list <- list()

# Loop through the first 20 pages to get 200 articles
for (page in 0:19) {
  url <- paste0("https://api.nytimes.com/svc/search/v2/articlesearch.json?",
                "q=", query, 
                "&begin_date=", gsub("-", "", format(start_date)), 
                "&end_date=", gsub("-", "", format(end_date)), 
                "&page=", page, 
                "&api-key=", api_key)
  
  response <- GET(url)
  content <- content(response, "text")
  json_data <- fromJSON(content)
  
  # Store articles in the list
  articles_list[[page + 1]] <- json_data$response$docs
  
  # NYT API rate limits; adjust sleep time as needed based on the API's guidelines
  Sys.sleep(6)
}

articles_df <- bind_rows(articles_list)

nrow(articles_df)

for_chat <- articles_df %>%
  select(abstract, lead_paragraph, headline)

for_chat_updated <- for_chat %>%
  mutate(headline = headline$main)

str(for_chat_updated)

for_chat_updated <- for_chat_updated %>%
  mutate(
    combined_text = paste(
      "Abstract:", for_chat_updated$abstract,
      "Lead Paragraph:", for_chat_updated$lead_paragraph,
      "Headline:", for_chat_updated$headline,
      sep = " "
    )
  )

indices <- expand.grid(row1 = 1:nrow(for_chat_updated), row2 = 1:nrow(for_chat_updated))
indices <- subset(indices, row1 < row2)

pairwise_df <- data.frame(text1 = for_chat_updated$combined_text[indices$row1],
                          text2 = for_chat_updated$combined_text[indices$row2])

submit_openai <- function(prompt, temperature = 0.2, n = 1) {
  res <- openai::create_chat_completion(model = "gpt-4-0125-preview",
                                        messages = prompt,
                                        temperature = temperature,
                                        n = n)
  Sys.sleep(1)
  res
}


create_prompt <- function(text1, text2) {
  res <- list(
    list(
      "role" = "system",
      "content" = "Please respond in the following format:
      Selection: name_of_selection (text1 or text2)
      Explanation: your reasoning for the selection"
    ),
    list(
      "role" = "user",
      "content" = stringr::str_c(
        'Please read the following information about articles from the New York Times. Each is about a protest. Out of the two examples, which article is framing the protestors in a more positive light?',
        'Text1: ', text1, 'Text2: ', text2
        )
    )
  )
  return(res)
}

prompts_list <- apply(pairwise_df, 1, function(row) create_prompt(row['text1'], row['text2']))

responses <- c()

for(i in 9501:9730) {
  responses[[i]] <- submit_openai(prompts_list[[i]])
  print(i)
}

create_prompt_2 <- function(text1, text2) {
  res <- list(
    list(
      "role" = "system",
      "content" = "Please respond in the following format:
      Selection: name_of_selection (text1 or text2)
      Explanation: your reasoning for the selection"
    ),
    list(
      "role" = "user",
      "content" = stringr::str_c(
        'Please read the following information about articles from the New York Times. Each is about a protest. Out of the two examples, which article is focusing on the protesters demands and the causes of the protest, as opposed to the protestors tactics? ',
        'Text1:', text1, 'Text2', text2
      )
    )
  )
  return(res)
}

prompts_list_prompt2 <- apply(pairwise_df, 1, function(row) create_prompt_2(row['text1'], row['text2']))

responses_prompt_2 <- c()

for(i in 9401:9730) {
  responses_prompt_2[[i]] <- submit_openai(prompts_list_prompt2[[i]])
  print(i)
}
### PROMPT LIST TWO
content_df <- data.frame(
  message_content = sapply(responses_prompt_2, function(x) x$choices$message.content)
)

user_prompts <- sapply(prompts_list_prompt2, function(prompt) prompt[[2]]$content)

content_df <- content_df %>%
  mutate(prompt = user_prompts)

content_df <- cbind(content_df, indices)

toMatch <- c("text1", "text2", "text 2")
pattern <- paste(toMatch, collapse = "|")

matches <- regexpr(pattern, content_df$message_content, ignore.case = TRUE)

content_df$selected_text <- ifelse(matches == -1, "None", regmatches(content_df$message_content, matches))
content_df$selected_text <- tolower(content_df$selected_text)
table(content_df$selected_text)

content_df$selected_text <- gsub("text 2", "text2", content_df$selected_text, ignore.case = TRUE)

table(content_df$selected_text)

content_df$preference <- ifelse(content_df$selected_text == "text1", 1, 0)

# bt_data <- data.frame(
#   player1 = as.factor(ifelse(content_df$preference == 1, content_df$text1, content_df$text2)),
#   player2 = as.factor(ifelse(content_df$preference == 1, content_df$text2, content_df$text1)),
#   outcome = content_df$preference
# )
# 
# str(bt_data)
# 
# 
# 
# library(BradleyTerry2)
# 
# common_levels <- union(levels(bt_data$player1), levels(bt_data$player2))
# bt_data$player1 <- factor(bt_data$player1, levels = common_levels)
# bt_data$player2 <- factor(bt_data$player2, levels = common_levels)
# 
# str(bt_data)
# 
# bt_data$player1 <- relevel(bt_data$player1, ref = 100)
# bt_data$player2 <- relevel(bt_data$player2, ref = 100)
# 
# bt_model <- BTm(player1 = bt_data$player1, player2 = bt_data$player2, outcome = bt_data$outcome, data = bt_data)
# 
# 
# # Check the model summary
# summary(bt_model)
# # Get scores from the model
# scores <- coef(bt_model)
# print(scores)
# library(stats)
# quartiles <- quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))
# quartiles
# 
# scores[140] <- 0
# 
# for_chat_updated$scores <- scores
# 
# ### PROMPT LIST ONE
# content_df_p1 <- data.frame(
#   message_content = sapply(responses, function(x) x$choices$message.content)
# )
# 
# user_prompts_p1 <- sapply(prompts_list, function(prompt) prompt[[2]]$content)
# 
# content_df_p1 <- content_df_p1 %>%
#   mutate(prompt = user_prompts_p1)
# 
# content_df_p1 <- cbind(content_df_p1, indices)
# 
# toMatch <- c("text1", "text2", "text 2")
# pattern <- paste(toMatch, collapse = "|")
# 
# matches_p1 <- regexpr(pattern, content_df_p1$message_content, ignore.case = TRUE)
# 
# content_df_p1$selected_text <- ifelse(matches == -1, "None", regmatches(content_df_p1$message_content, matches_p1))
# content_df_p1$selected_text <- tolower(content_df_p1$selected_text)
# table(content_df_p1$selected_text)
# 
# content_df_p1$preference <- ifelse(content_df_p1$selected_text == "text1", 1, 0)
# 
# bt_data_p1 <- data.frame(
#   player1 = as.factor(ifelse(content_df_p1$preference == 1, content_df_p1$text1, content_df_p1$text2)),
#   player2 = as.factor(ifelse(content_df_p1$preference == 1, content_df_p1$text2, content_df_p1$text1)),
#   outcome = 1
# )
# 
# str(bt_data_p1)
# 
# 
# 
# library(BradleyTerry2)
# 
# # common_levels <- union(levels(bt_data$player1), levels(bt_data$player2))
# # bt_data$player1 <- factor(bt_data$player1, levels = common_levels)
# # bt_data$player2 <- factor(bt_data$player2, levels = common_levels)
# 
# # str(bt_data)
# 
# bt_data_p1$player1 <- relevel(bt_data_p1$player1, ref = 100)
# bt_data_p1$player2 <- relevel(bt_data_p1$player2, ref = 100)
# 
# bt_model_p1 <- BTm(player1 = bt_data_p1$player1, player2 = bt_data_p1$player2, outcome = bt_data_p1$outcome, data = bt_data_p1)
# 
# 
# # Check the model summary
# summary(bt_model_p1)
# # Get scores from the model
# scores_p1 <- coef(bt_model_p1)
# print(scores_p1)
# library(stats)
# quartiles_p1 <- quantile(scores_p1, probs = c(0, 0.25, 0.5, 0.75, 1))
# quartiles_p1
# 
# scores_p1 <- as.data.frame(scores_p1)
# 
# # scores[140] <- 0
# 
# # for_chat_updated$scores <- scores
# 
# 
# 
# 
