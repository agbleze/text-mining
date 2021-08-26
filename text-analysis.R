### text files
library(pdftools)
library(magrittr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)
library(tidygraph)


file_vector <- list.files(path = "Data")
file_vector %>% head()
grepl(".pdf", file_vector)

## select only pdf
pdf_list <- file_vector[grepl("pdf", file_vector)]
head(pdf_list)
pdf_text("Data/BUSINESSCENTER.pdf")%>%
  strsplit(split = "\n")

corpus_raw <- data.frame("company" = c(), "text" = c())

for (i in 1:length(pdf_list)){
  print(i)
  pdf_text(paste("data/", pdf_list[i], sep = ""))%>%
    strsplit("\n") -> document_text
  data.frame("company" = gsub(x = pdf_list[i], pattern = ".pdf", replacement = ""),
             "text" = document_text, stringsAsFactors = FALSE) -> document
  colnames(document) <- c("company", "text")
  corpus_raw <- rbind(corpus_raw, document)
}
corpus_raw%>%head()

### removing irrelevant information
corpus_raw %>%
  filter(!grepl("12.05.2017", text)) %>%
  filter(!grepl("business profile", text)) %>%
  filter(!grepl("comments", text)) %>%
  filter(!grepl("1", text)) -> corpus

corpus %>%
  filter(!grepl(c("date of foundation"), text)) %>%
  filter(!grepl(c("industry"), text)) %>%
  filter(!grepl(c("share holders"), text)) -> comments

corpus %>%
  filter(grepl(("date of foundation"), text) | grepl(("industry"), text) | grepl(("share holders"), text)) -> information


## tokeninzing
comments %>%
  unnest_tokens(word, text) -> comments_tidy

############   sentiment analysis  ##############
get_sentiments("bing")

comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(company, sentiment)%>%
  spread(sentiment, n, fill = 0)%>%
  mutate(sentiment = positive - negative) -> comments_sentiment

ggplot(comments_sentiment, aes(x = sentiment)) + geom_histogram()

##### wordcloud  ###
comments_tidy %>%
  count(word) %>%
  with(wordcloud(word, n))



####### remove stop_words  and plot wordcloud ############
comments_tidy %>%
  filter(!word %in% stop_words$word) %>%
  count(word) %>%
  with(wordcloud(word, n))


#### find context in text analysis with n-grams  ######
comments %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) -> bigram_comments

## FILTER bigram from stop_words and count them
bigram_comments %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

###### trigrams
comments %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) -> trigram_comments

trigram_comments %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) -> trigram_words

#############  network analysis  ##
information %>%
  filter(grepl("industry", text))

### clean in text col to have industry
information %>%
  filter(grepl("industry", text)) %>%
  mutate(industry = gsub("industry: ", "", text)) -> industries

## plot results of industries
industries %>%
  count(industry) %>%
  filter(n > 1) %>%
  ggplot(aes(x = industry, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

#### filtering shareholder info
information %>%
  filter(grepl("share holders", text)) %>%
  mutate(shareholders = gsub("share holders: ", "", text))

### separate each shareholder into diff col 
information %>%
  filter(grepl("share holders", text)) %>%
  mutate(shareholders = gsub("share holders: ", "", text)) %>%
  separate(col = shareholders, into = c("first", "second", "third"), sep = ";") %>%
  ## gather 
  gather(key = "number", value = "shareholder", -company, -text) %>%
  ## filter those not shareholders
  filter(!is.na(shareholder)) %>%
  select(company, shareholder) -> shareholders


#### visualizing the graph
# obtain edge list from data
graph_from_data_frame(shareholders) -> graph_object
graph_object%>%
  ggraph() +
  geom_edge_link(alpha = .2) +
  geom_node_point(alpha = .3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE) + theme_graph()

## cal degree for nodes
deg <- degree(graph_object, mode = "all")
# take node vertice of graph object and create attribute size and assign 3 time of the degree
V(graph_object)$size <- deg*3


#plot the degree of graph
set.seed(30)
graph_object %>%
  ggraph() + geom_edge_link(alpha = .2) + geom_node_point(aes(size = size), alpha = .3) + 
  geom_node_text(aes(label = name, size = size), vjust = 1, hjust = 1, check_overlap = FALSE) + theme_graph()
