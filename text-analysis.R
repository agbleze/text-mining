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
