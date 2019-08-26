# walmart cooccurrence

# nike co-occurrence

df <- read_csv("walmart_reviews.csv")


negative <- df %>%
  filter(stars <= 2) %>%
  mutate(review = str_replace(review,"customer service","customerservice")) %>%
  unnest_tokens(word, review) %>%
  mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words) %>%
  filter(!word %in% "walmart") %>%
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
