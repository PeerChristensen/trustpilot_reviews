# nike co-occurrence

library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(SnowballC)
library(udpipe)

df <- read_csv("nike_reviews.csv") %>%
  mutate(review = str_replace(review,"customer service","customerservice"))

#ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

# nike_tagged <- udpipe(ud_english, x = df$review,doc_id = df$reviewId)
# 
# nike_tagged <- nike_tagged %>%
#   select(doc_id,token,lemma,upos)
# 
# write_csv(nike_tagged,"nike_tagged.csv")

df <- df %>%
  filter(stars >= 4 |stars <= 2) %>%
  mutate(valence = if_else(stars >=4,"Good","Bad")) %>%
  select(doc_id = reviewId,valence)

nike_tagged <- read_csv("nike_tagged.csv")

nike_tagged <- nike_tagged %>%
  inner_join(df) %>%
  #mutate(token = tolower(token)) %>%
  #mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words, by=c("token" = "word")) %>%
  filter(upos == "ADJ"|upos =="NOUN"|upos=="VERB"|upos=="PROPN"|token =="customerservice",
         !token %in% "nike") %>%
  mutate(lemma = recode(lemma,customerservice = "customer service",service ="customer service"))
       
negative <- nike_tagged %>%
  filter(valence == "Bad") %>%
  add_count(lemma) %>%
  filter(n>30) %>%
  pairwise_cor(lemma, doc_id, sort = TRUE)

# plots
set.seed(611)

negative_plot <- negative %>%
  top_n(120) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 edge_colour="indianred",show.legend = F) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,size=6,
                 point.padding = unit(0.2, "lines"),
                 family = "Helvetica") +
  theme_graph()
  

negative_plot
ggsave("nike_network_bad_reviews.png",width=17,height=12)
