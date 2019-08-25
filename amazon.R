# amazon reviews, co-occurrence

#library(trustpilotR)
#df <- get_reviews("https://uk.trustpilot.com/review/www.amazon.com", page_lim = 260)
#write_csv(df, "amazon.csv")

library(tidyverse)
library(tidytext)
library(jtools)
library(gridExtra)
library(widyr)
library(igraph)
library(ggraph)

# SET THEME
my_theme <- function() {
  theme_apa(legend.pos = "none") +
    theme(panel.background = element_rect(fill = "gray96", colour = "gray96")) +
    theme(plot.background = element_rect(fill = "gray96", colour = "gray96")) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    theme(panel.border = element_blank()) +                       # facet border
    theme(strip.background = element_blank())                 # facet title background
}

df <- read_csv("amazon.csv")

positive <- df %>%
  filter(stars >= 4) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  filter(n > 70) %>% 
  pairwise_cor(word, consumerId, sort = TRUE) %>%
  filter(correlation >.2)

# plots
set.seed(611)

positive_plot <- positive %>%
  #top_n(100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

positive_plot

## count

positive <- df %>%
  filter(stars >= 4) %>%
  mutate(review = str_replace(review,"customer service","customerservice")) %>%
  unnest_tokens(word, review) %>%
  mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words) %>%
  filter(!word %in% "amazon") %>%
  add_count(word) %>%
  pairwise_count(word, consumerId, sort = TRUE)

# plots
set.seed(611)

positive_plot <- positive %>%
  top_n(200) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

positive_plot

# negative

negative <- df %>%
  filter(stars <= 2) %>%
  mutate(review = str_replace(review,"customer service","customerservice")) %>%
  unnest_tokens(word, review) %>%
  mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words) %>%
  filter(!word %in% "amazon") %>%
  pairwise_count(word, consumerId, sort = TRUE)

# plots
set.seed(611)

negative_plot <- negative %>%
  top_n(200) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),edge_colour="steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

negative_plot

# customer service corr

parts_of_speech <- parts_of_speech %>%
  add_row(word = "customerservice",pos = "Noun")

badCS_cor <- df %>%
  filter(stars <= 2) %>%
  mutate(review = str_replace(review,"customer service","customerservice")) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  inner_join(parts_of_speech) %>%
  filter(!word == "amazon",
         pos == "Noun") %>%
  pairwise_cor(word,consumerId,sort=T) %>%
  filter(item1 == "customerservice",
         !item2 %in% c("customer","service"))

badCS_cor

df %>%
  filter(stars <= 2) %>%
  mutate(review = str_replace(review,"customer service","customerservice")) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  inner_join(parts_of_speech) %>%
  filter(!word == "amazon") %>%
  pairwise_cor(word,consumerId,sort=T) %>%
  filter(item1 == "customerservice",
         !item2 %in% c("customer","service")) %>%
  inner_join(parts_of_speech,by=c("item2"="word")) %>%
  filter(pos =="Adjective")

