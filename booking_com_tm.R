
library(tidyverse)
library(tm)
library(tidytext)
library(stm)
library(quanteda)
library(furrr)
library(scales)
library(SnowballC)
library(textstem)
library(hrbrthemes)
library(ggthemes)
library(gridExtra)


df <- read_csv("booking_com.csv")

df              <- df                      %>%
  filter(!consumerName %in% c("Marie Donno", "Terry Dom")) %>%
  unnest_tokens(word, review)              %>%
  mutate(word = lemmatize_words(word)) %>%
  anti_join(stop_words)                    %>%
  add_count(word)                          %>%
  select(-n)                               %>%
  dplyr::filter(!word %in% c("booking.com","book"),
                !str_detect(word,"[0-9]"))

n_topics = seq(4,6,2) # change to seq

# bad reviews

df_sparse1 <- df                  %>%
  filter(stars == 1) %>%
  mutate(review_id = factor(reviewId)) %>%
  count(consumerName, word)          %>%
  cast_dtm(consumerName,word,n)

df_sparse1 = removeSparseTerms(df_sparse1, 0.99)

df_sparse1 <- as.dfm(df_sparse1)

rowTotals <- apply(df_sparse1 , 1, sum) #Find the sum of words in each Document
df_sparse1  <- df_sparse1[rowTotals> 0, ] 

many_models_stm1 <- data_frame(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse1, K = ., verbose = FALSE)))

heldout <- make.heldout(df_sparse1)

k_result1 <- many_models_stm1 %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, df_sparse1),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, df_sparse1),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result1 %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")

excl_sem_plot <- k_result1                    %>%
  select(K, exclusivity, semantic_coherence) %>%
  #filter(K %in% seq(2,15))                   %>%
  unnest()                                   %>%
  mutate(K = as.factor(K))                   %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity") 

excl_sem_plot

topic_model_stm1 <- k_result1 %>% 
  dplyr::filter(K == 6)        %>% 
  pull(topic_model)           %>% 
  .[[1]]

td_beta1 <- tidy(topic_model_stm1)

top_terms1 <- td_beta1  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(5, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma1 <- tidy(topic_model_stm1, matrix = "gamma",
                 document_names = rownames(df_sparse1))

gamma_terms1 <- td_gamma1              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms1, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot1 <- gamma_terms1 %>%
  #top_n(3, gamma)      %>% # n topics
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,width=.7) +
  geom_text(hjust = -.05, vjust=0, size = 8, family = "Roboto Condensed") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.6),
                     labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = expression(gamma),
       title = "1-star review topics") +
  #scale_fill_viridis_d(begin=.3) +
  theme_ipsum_rc() +
  scale_fill_tableau() +
  theme(plot.margin = margin(3,1.5, 3,1.5, "cm"),
        plot.title = element_text(size=30),
        axis.text.y = element_text(size = 24,
                                   margin = margin(r = .3, unit = "cm")),
        axis.text.x = element_text(size = 20),
        panel.grid = element_blank())

stm_plot1

# good reviews

df_sparse5 <- df                  %>%
  filter(stars == 5) %>%
  mutate(review_id = factor(reviewId)) %>%
  count(consumerName, word)          %>%
  cast_dtm(consumerName,word,n)

df_sparse5 = removeSparseTerms(df_sparse5, 0.99)

df_sparse5 <- as.dfm(df_sparse5)

rowTotals <- apply(df_sparse5 , 1, sum) #Find the sum of words in each Document
df_sparse5  <- df_sparse5[rowTotals> 0, ] 

many_models_stm5 <- data_frame(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse5, K = ., verbose = FALSE)))

heldout <- make.heldout(df_sparse5)

k_result5 <- many_models_stm5 %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, df_sparse5),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, df_sparse5),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result5 %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")

excl_sem_plot <- k_result5                    %>%
  select(K, exclusivity, semantic_coherence) %>%
  #filter(K %in% seq(2,15))                   %>%
  unnest()                                   %>%
  mutate(K = as.factor(K))                   %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity") 

excl_sem_plot

topic_model_stm5 <- k_result5 %>% 
  dplyr::filter(K == 6)        %>% 
  pull(topic_model)           %>% 
  .[[1]]

td_beta5 <- tidy(topic_model_stm5)

top_terms5 <- td_beta5  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(5, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma5 <- tidy(topic_model_stm5, matrix = "gamma",
                  document_names = rownames(df_sparse5))

gamma_terms5 <- td_gamma5              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms5, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot5 <- gamma_terms5 %>%
  #top_n(3, gamma)      %>% # n topics
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,width=.7) +
  geom_text(hjust = -.05, vjust=0, size = 8, family = "Roboto Condensed") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.62),
                     labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = expression(gamma),
       title = "5-star review topics") +
  #scale_fill_viridis_d(begin=.3) +
  theme_ipsum_rc() +
  scale_fill_tableau() +
  theme(plot.margin = margin(3,1.5, 3,1.5, "cm"),
        plot.title = element_text(size=30),
        axis.text.y = element_text(size = 24,
                                   margin = margin(r = .3, unit = "cm")),
        axis.text.x = element_text(size = 20),
        panel.grid = element_blank())

stm_plot5

g=arrangeGrob(stm_plot5,stm_plot1,ncol=2)
ggsave("booking_com_tm.png",g,width = 24,height=10)
ggsave("booking_com_tm2.png",g,width = 24,height=12) # best
