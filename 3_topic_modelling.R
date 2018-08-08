# TOPIC MODELLING OF 3 REVIEWS SCRAPED FROM TRUSTPILOT.DK
# PEER CHRISTENSEN
# AUGUST 2018

library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(magrittr)
library(happyorsad)
library(jtools)
library(gridExtra)

# SET THEME
my_theme <- function() {
  theme_apa(legend.pos = "none") +
    theme(panel.background = element_rect(fill = "gray96", colour = "gray96")) +
    theme(plot.background = element_rect(fill = "gray96", colour = "gray96")) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    theme(panel.border = element_blank()) +                       # facet border
    theme(strip.background = element_blank())                 # facet title background
}

# LOAD & CLEAN
df = read_csv2("3reviewsB.csv")
df %<>% 
  select(-X1) %>%
  filter(Name != "John M. Sebastian") 

# add sentiments, remove stop words
df %<>%
  mutate(Sentiment = map_int(df$Review,happyorsad,"da")) %>% # compute sentiment score using AFINN
  mutate(Review = tolower(Review)) %>%
  mutate(Review = removeWords(Review, c("så", "3", "kan","få","får","fik", stopwords("danish"))))

#save data
write_csv(df, "3_tm_data")

################################################################

df = read_csv("3_tm_data")

#  Plot distribution with median
df %>% 
  ggplot(aes(x=Sentiment)) + 
  geom_density(size=1) +
  geom_vline(xintercept = median(df$Sentiment), colour = "indianred", linetype = "dashed", size =1) +
  ggplot2::annotate("text", x = 15, y = 0.06, label = paste("median = ", median(df$Sentiment)), colour = "indianred") +
  my_theme() +
  xlim(-40,40)

# topic modelling for positive  reviews
df_pos <- df %>%
  filter(Sentiment > 1) %>%
  select(-Sentiment) %>%
  unnest_tokens(word, Review)

words_pos <- df_pos %>%
  count(Name, word, sort = TRUE) %>%
  ungroup()

reviewDTM_pos <- words_pos %>%
  cast_dtm(Name, word, n)

reviewLDA_pos <- LDA(reviewDTM_pos, k = 4, control = list(seed = 347))

#n docs assigned to topics
tibble(topics(reviewLDA_pos)) %>%
  group_by(`topics(reviewLDA_pos)`) %>%
  count() %>%
  ggplot(aes(x = factor(`topics(reviewLDA_pos)`), y = n)) +
  geom_col(fill = "steelblue") +
  my_theme() +
  xlab("Topic")

#per-topic per-word probabilities
topics_pos <- tidy(reviewLDA_pos, matrix = "beta")

#top terms per topic
topTerms_pos <- topics_pos %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number())) # necessary for ordering words

# topic modelling for negative  reviews
df_neg <- df %>%
  filter(Sentiment < 1) %>%
  select(-Sentiment) %>%
  unnest_tokens(word, Review)

words_neg <- df_neg %>%
  count(Name, word, sort = TRUE) %>%
  ungroup()

reviewDTM_neg <- words_neg %>%
  cast_dtm(Name, word, n)

reviewLDA_neg <- LDA(reviewDTM_neg, k = 4, control = list(seed = 347))

#n docs assigned to topics
tibble(topics(reviewLDA_neg)) %>%
  group_by(`topics(reviewLDA_neg)`) %>%
  count() %>%
  ggplot(aes(x = factor(`topics(reviewLDA_neg)`), y = n)) +
  geom_col(fill = "indianred") +
  my_theme() +
  xlab("Topic")

#per-topic per-word probabilities
topics_neg <- tidy(reviewLDA_neg, matrix = "beta")

#top terms per topic
topTerms_neg <- topics_neg %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number())) # necessary for ordering words

# PLOTS

#plot positive
plot_pos <- topTerms_pos %>%
  ggplot(aes(order, beta)) +
  ggtitle("Positive review topics") +
  geom_col(show.legend = FALSE, fill = "steelblue") +
  scale_x_continuous(
    breaks = topTerms_pos$order,
    labels = topTerms_pos$term,
    expand = c(0,0))+
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,0.02)) +
  my_theme() +
  theme(axis.title=element_blank())

#plot negative
plot_neg <- topTerms_neg %>%
  ggplot(aes(order, beta, fill = factor(topic))) +
  ggtitle("Negative review topics") +
  geom_col(show.legend = FALSE, fill = "indianred") +
  scale_x_continuous(
    breaks = topTerms_neg$order,
    labels = topTerms_neg$term,
    expand = c(0,0))+
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,0.02)) +
  my_theme() +
  theme(axis.title=element_blank())

combined_plot = arrangeGrob(plot_pos,plot_neg, ncol = 1)
combined_plot
ggsave("combined_plot.png", combined_plot, height = 10, width = 12)

# sentiment score, try with map()
#sentiment = NULL
#for (i in 1:nrow(df)) {
#  sentiment[i] = happyorsad(df$Review[i], "da")
#}

