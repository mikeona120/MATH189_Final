#R Code for Final Project by Mike Ona
#Text Summary of Projects
pdf("TextSummary.pdf")

install.packages("tidytext")
install.packages("ggplot2")
install.packages("wordcloud")
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(scales)
# library(janeaustenr)

projects <- read.csv("Projects.csv", header=TRUE, sep=",")

projects_funded <- projects[projects$Project.Current.Status == "Fully Funded", ]
projects_expired <- projects[projects$Project.Current.Status == "Expired", ]

#create dataframe
funded_title <- data_frame(content = projects_funded$Project.Title)
expired_title <- data_frame(content = projects_expired$Project.Title)
funded_essay <- data_frame(content = projects_funded$Project.Essay)
expired_essay <- data_frame(content = projects_expired$Project.Essay)
funded_desc <- data_frame(content = projects_funded$Project.Short.Description)
expired_desc <- data_frame(content = projects_expired$Project.Short.Description)
funded_needs <- data_frame(content = projects_funded$Project.Need.Statement)
expired_needs <- data_frame(content = projects_expired$Project.Need.Statement)
#convert to character
funded_title[] <- lapply(funded_title, as.character)
expired_title[] <- lapply(expired_title, as.character)
funded_essay[] <- lapply(funded_essay, as.character)
expired_essay[] <- lapply(expired_essay, as.character)
funded_desc[] <- lapply(funded_desc, as.character)
expired_desc[] <- lapply(expired_desc, as.character)
funded_needs[] <- lapply(funded_needs, as.character)
expired_needs[] <- lapply(expired_needs, as.character)
#create dataframe of characters
funded_title_df <- data_frame(content = funded_title$content)
expired_title_df <- data_frame(content = expired_title$content)
funded_essay_df <- data_frame(content = funded_essay$content)
expired_essay_df <- data_frame(content = expired_essay$content)
funded_desc_df <- data_frame(content = funded_desc$content)
expired_desc_df <- data_frame(content = expired_desc$content)
funded_needs_df <- data_frame(content = funded_needs$content)
expired_needs_df <- data_frame(content = expired_needs$content)

#one-token-per-row
funded_title_df<-funded_title_df %>%
  unnest_tokens(word, content)
expired_title_df<-expired_title_df %>%
  unnest_tokens(word, content)
funded_essay_df<-funded_essay_df %>%
  unnest_tokens(word, content)
expired_essay_df<-expired_essay_df %>%
  unnest_tokens(word, content)
funded_desc_df<-funded_desc_df %>%
  unnest_tokens(word, content)
expired_desc_df<-expired_desc_df %>%
  unnest_tokens(word, content)
funded_needs_df<-funded_needs_df %>%
  unnest_tokens(word, content)
expired_needs_df<-expired_needs_df %>%
  unnest_tokens(word, content)

#remove stop words
data(stop_words)
funded_title_df <- funded_title_df %>%
  anti_join(stop_words, by=c("word"="word"))
expired_title_df <- expired_title_df %>%
  anti_join(stop_words, by=c("word"="word"))
funded_essay_df <- funded_essay_df %>%
  anti_join(stop_words, by=c("word"="word")) 
funded_essay_df<-funded_essay_df[!(funded_essay_df$word=="donotremoveessaydivider"),]
expired_essay_df <- expired_essay_df %>%
  anti_join(stop_words, by=c("word"="word"))
expired_essay_df<-expired_essay_df[!(expired_essay_df$word=="donotremoveessaydivider"),]
funded_desc_df <- funded_desc_df %>%
  anti_join(stop_words, by=c("word"="word"))
expired_desc_df <- expired_desc_df %>%
  anti_join(stop_words, by=c("word"="word"))
funded_needs_df <- funded_needs_df %>%
  anti_join(stop_words, by=c("word"="word"))
expired_needs_df <- expired_needs_df %>%
  anti_join(stop_words, by=c("word"="word"))

#lengths
n1 <- dim(funded_title_df)[1]
n2 <- dim(expired_title_df)[1]
n3 <- dim(funded_essay_df)[1]
n4 <- dim(expired_essay_df)[1]
n5 <- dim(funded_desc_df)[1]
n6 <- dim(expired_desc_df)[1]
n7 <- dim(funded_needs_df)[1]
n8 <- dim(expired_needs_df)[1]

#word count
word_count1 <- funded_title_df %>%
  count(word, sort = TRUE)
word_count2 <- expired_title_df %>%
  count(word, sort = TRUE)
word_count3 <- funded_essay_df %>%
  count(word, sort = TRUE)
word_count4 <- expired_essay_df %>%
  count(word, sort = TRUE)
word_count5 <- funded_desc_df %>%
  count(word, sort = TRUE)
word_count6 <- expired_desc_df %>%
  count(word, sort = TRUE)
word_count7 <- funded_needs_df %>%
  count(word, sort = TRUE)
word_count8 <- expired_needs_df %>%
  count(word, sort = TRUE)

# #histogram
# funded_title_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 700) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 2000)
# expired_title_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 240) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 2000) 
# funded_essay_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 18900) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 200000)
# expired_essay_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 7600) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 200000)
# funded_desc_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 2960) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 25000)
# expired_desc_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 1040) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 25000)
# funded_needs_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 1700) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 30000)
# expired_needs_df %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 670) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip() +
#   ylim(0, 30000)

#histogram - percents
funded_title_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 700) %>%
  mutate(word = reorder(word, n/n1)) %>%
  ggplot(aes(word, n/n1)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 2000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Titles - Funded")
expired_title_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 240) %>%
  mutate(word = reorder(word, n/n2)) %>%
  ggplot(aes(word, n/n2)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 2000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Titles - Expired") 
funded_essay_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 18300) %>%
  mutate(word = reorder(word, n/n3)) %>%
  ggplot(aes(word, n/n3)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 200000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Essays - Funded")
expired_essay_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 7400) %>%
  mutate(word = reorder(word, n/n4)) %>%
  ggplot(aes(word, n/n4)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 200000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Essays - Expired")
funded_desc_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 2960) %>%
  mutate(word = reorder(word, n/n5)) %>%
  ggplot(aes(word, n/n5)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 25000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Descriptions - Funded")
expired_desc_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 1040) %>%
  mutate(word = reorder(word, n/n6)) %>%
  ggplot(aes(word, n/n6)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 25000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Descriptions - Expired")
funded_needs_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 1700) %>%
  mutate(word = reorder(word, n/n7)) %>%
  ggplot(aes(word, n/n7)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 30000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Need Statements - Funded")
expired_needs_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 670) %>%
  mutate(word = reorder(word, n/n8)) %>%
  ggplot(aes(word, n/n8)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylim(0, 30000) +
  scale_y_continuous(labels=percent) +
  labs(x = "word",y = "Percent", title = "Project Need Statements - Expired")


#sentiment analysis
#set sentiment
nrc_sentiment <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

#get counts of each sentiment
funded_title_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
expired_title_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
funded_essay_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
expired_essay_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
funded_desc_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
expired_desc_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
funded_needs_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)
expired_needs_df %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)

#most common positive and negative words
bing_word_counts <- funded_title_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Titles - Funded") +
  coord_flip()

bing_word_counts <- expired_title_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Titles - Expired") +
  coord_flip()

bing_word_counts <- funded_essay_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Essays - Funded") +
  coord_flip()

bing_word_counts <- expired_essay_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Essays - Expired") +
  coord_flip()

bing_word_counts <- funded_desc_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Descriptions - Funded") +
  coord_flip()

bing_word_counts <- expired_desc_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Descriptions - Expired") +
  coord_flip()

bing_word_counts <- funded_needs_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Need Statements - Funded") +
  coord_flip()

bing_word_counts <- expired_needs_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "word", title = "Project Need Statements - Expired") +
  coord_flip()

#word clouds of most common words
funded_title_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
expired_title_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
funded_essay_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
expired_essay_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
funded_desc_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
expired_desc_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
funded_needs_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
expired_needs_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#bigrams
# library(janeaustenr)
# needs_bigrams <- austen_books() %>%
#   unnest_tokens(bigram, tidy_needs_df, token = "ngrams", n = 2)
dev.off()