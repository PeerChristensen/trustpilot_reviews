# 3 correlation networks

library(tidyverse)
library(trustpilotR)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)


#df <- get_reviews("https://www.trustpilot.com/review/www.booking.com",page_lim = 260)

df <- read_csv("booking_com.csv")

df1 <- df %>%
  filter(stars == 1)

df1 <- df1 %>%
  unnest_tokens(word, review)               %>%
  anti_join(stop_words)                     %>%
  dplyr::filter(!word %in% "booking.com",
                !str_detect(word,"[0-9]+")) %>%
  add_count(word)                           %>%
  dplyr::filter(n > 25)                     %>%
  select(-n)

df5 <- df %>%
  filter(stars == 5)

df5 <- df5 %>%
  unnest_tokens(word, review)              %>%
  anti_join(stop_words)                    %>%
  dplyr::filter(!word %in% "booking.com",
                !str_detect(word,"[0-9]+")) %>%
  add_count(word)                          %>%
  dplyr::filter(n > 25)                    %>%
  select(-n)

# Pairwise word co-occurence
word_pairs1 <- df1 %>% 
  pairwise_cor(word, consumerName, sort = TRUE)

word_pairs5 <- df5 %>%
  pairwise_cor(word, consumerName, sort = TRUE)

# plots
set.seed(611)

pairs_plot1 <- word_pairs1 %>%
  dplyr::filter(correlation > .2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "steelblue") +
  ggtitle("1 Star ratings") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pairs_plot1

set.seed(611)

pairs_plot5 <- word_pairs5 %>%
  dplyr::filter(correlation > .2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "steelblue") +
  ggtitle("5 Star ratings") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pairs_plot5

