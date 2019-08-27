
library(tidyverse)
library(tidytext)
library(udpipe)

df <- read_csv("amazon.csv") %>%
  mutate(review = str_replace(review,"customer service","customerservice"))

#language_nodel <- udpipe_download_model(language = "english-ewt")
ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

#amazon_tagged <- udpipe(df$review, object = ud_english)

tagged <- udpipe(ud_english, x = df$review,doc_id = df$reviewId)

tagged <- tagged %>%
  select(doc_id,token,lemma,upos)

df <- df %>%
  filter(stars >= 4 |stars <= 2) %>%
  mutate(valence = if_else(stars >=4,"Good","Bad")) %>%
  select(doc_id = reviewId,valence)

tagged <- tagged %>%
  inner_join(df) %>%
  #filter(pos == "Noun") %>%
  #mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words, by=c("token" = "word")) %>%
  filter(upos == "NOUN",
         token !="PMJimlise")

tagged %>%
  count(lemma, valence) %>%
  filter(sum(n) >= 20) %>%
  spread(valence, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(list(~(. + 1) / sum(. + 1)), -lemma) %>%
  mutate(logratio = log2(Good / Bad)) %>%
  arrange(desc(logratio)) %>%
  group_by(logratio > 5) %>%
  top_n(30, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(lemma, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Good / Bad log ratio") +
  scale_fill_manual(name = "", labels = c("Good", "Bad"),
                    values = c("red", "lightblue"))

#### tagged

tagged <- read_csv("amazon_reviews_tagged.csv")
