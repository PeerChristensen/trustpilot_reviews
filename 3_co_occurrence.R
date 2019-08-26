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

df = read_csv("3_tm_data")

df_pos <- df %>%
  filter(Sentiment > 1) %>%
  select(-Sentiment) %>%
  unnest_tokens(word, Review)

# Pairwise word co-occurence
word_pairs_pos <- df_pos %>% 
  pairwise_count(word, Name, sort = TRUE)

word_pairs_neg <- df_neg %>%
  pairwise_count(word, Name, sort = TRUE)

# plots
set.seed(611)

pairs_plot_pos <- word_pairs_pos %>%
  filter(n >= 140) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pairs_plot_neg <- word_pairs_neg %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "indianred") +
  ggtitle("Negative word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

grid.arrange(pairs_plot_pos, pairs_plot_neg, ncol = 2)

# word pair correlations
df_pos %>% group_by(word) %>%
  #filter(n() >= 50) %>%
  pairwise_cor(word, Name, sort = TRUE)  

df_neg %>% group_by(word) %>%
  filter(n() >= 50) %>%
  pairwise_cor(word, Name, sort = TRUE)  %>%
  top_n(7)


