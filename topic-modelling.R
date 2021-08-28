### topic modelling
## Latent Dirichlet Allocation (LDA)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gutenbergr)
library(tidyverse)

data("AssociatedPress")
AssociatedPress

#LDA topic model with two topics
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

## extract per-topic-per-word probabilities
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

## taking top 10 terms
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

## plot results of most ocuring terms in each topic
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = F) + facet_wrap(~ topic, scales = "free") +
  coord_flip()

## terms with the greatest diff in beta b/T topic 1 and topic 2
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  ggplot(aes(x = term, y = log_ratio)) + geom_col(show.legend = F) + coord_flip()


## DOCUMENT-topic probabilities
## estimate the per-document-per-topic probabilities
ap_document <- tidy(ap_lda, matrix = "gamma")
ap_document

## tidy document-term matrix
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

