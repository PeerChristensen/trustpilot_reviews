
# log ratio per PoS, lemmatized
library(tidyverse)
library(tidytext)
#library(udpipe)


#language_nodel <- udpipe_download_model(language = "english-ewt")
#ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")


#tagged <- udpipe(ud_english, x = df$review,doc_id = df$reviewId)

#tagged <- tagged %>%
#  select(doc_id,token,lemma,upos)

#write_csv(tagged,"amazon_tagged.csv")

df <- read_csv("amazon.csv") %>%
  mutate(review = str_replace(review,"customer service","customerservice"))

df <- df %>%
  filter(stars >= 4 |stars <= 2) %>%
  mutate(valence = if_else(stars >=4,"Good","Bad")) %>%
  select(doc_id = reviewId,valence)

tagged <- read_csv("amazon_tagged.csv")

tagged <- tagged %>%
  inner_join(df) %>%
  mutate(token = tolower(token)) %>%
  #mutate(word = str_replace(word,"customerservice","customer service")) %>%
  anti_join(stop_words, by=c("token" = "word")) %>%
  filter(upos == "ADJ"|upos =="NOUN"|upos=="VERB",
         token !="pmjimlise")

tagged <-tagged %>% 
  group_by(upos) %>%
  count(lemma, valence) %>%
  filter(sum(n) >= 20) %>%
  spread(valence, n, fill = 0) %>%
  mutate_each(list(~(. + 1) / sum(. + 1)), -lemma) %>%
  mutate(logratio = log2(Good / Bad)) %>%
  arrange(desc(logratio)) %>%
  group_by(upos,logratio > 5) %>%
  top_n(20, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder_within(lemma, logratio,upos,),
         word2 = str_split(word,"_") %>% map(1)%>% unlist())

tagged %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  facet_wrap(~upos,scales="free") +
  coord_flip() +
  ylab("Good / Bad log ratio") +
  scale_x_discrete(breaks = t$word,labels = t$word2) +
  scale_fill_manual(name = "", labels = c("Good", "Bad"),
                    values = c("red", "lightblue"))


