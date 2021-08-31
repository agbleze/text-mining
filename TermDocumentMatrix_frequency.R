######## TEXT Mining
##### TermDocumentMatrix (TDM) frequency finding

##set options
options(stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL", "C")

## load libraries
library(stringi)
library(stringr)
library(qdap)
library(tm)

library(readr)
text.df <- read_csv("/Volumes/SAMSUNG/text_mining-master 2/oct_delta.csv")

######### tweeters   
tweets <- data.frame(doc_id = seq(1:nrow(text.df)), text = text.df$text)

# returns NA instead of tolower error
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error <- tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')

## customized function for transformation for text minining
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

# create volatile corpus
corpus <- VCorpus(DataframeSource(tweets)) 
corpus <- clean.corpus(corpus)
as.list(corpus)

tm.definition <- 'Txt mining is the process of distilling actionable insyghts from text.'
which_misspelled(tm.definition)
check_spelling_interactive(tm.definition)

## custome function to replace misspelled words
fix.text <- function(myStr){
  check <- check_spelling(myStr)
  splitted <- strsplit(myStr, split = ' ')
  for ( i in 1:length(check$row)) {
    splitted[[check$row[i]]][as.numeric(check$word.no[i])] = check$suggestion[i]
  }
  df <- unlist(lapply(splitted, function(x) paste(x, collapse = ' ')))
  return(df)
}

fix.text(tm.definition)

## convert to TermDocumentMatrix
tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTf))
tdm.tweets.m <- as.matrix(tdm)

# check dimension
dim(tdm.tweets.m)
## check result of some rows and columns
tdm.tweets.m[2250:2255, 1340:1342]

# find frequency of the terms
term.freq <- rowSums(tdm.tweets.m)
freq.df <- data.frame(word = names(term.freq), frequency = term.freq)
freq.df <- freq.df[order(freq.df[,2], decreasing = T),]
freq.df[1:10,]
                      
                      
                      
                      
 ## convert to factor so bar graph plotted can be sorted
freq.df$word <- factor(freq.df$word, levels = unique(as.character(freq.df$word)))
ggplot(freq.df[1:20, ], aes(x = word, y = frequency)) + geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() + theme_gdocs() + geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 5.0)

##### word association
associations <- findAssocs(tdm, 'apologies', 0.11)
associations <- as.data.frame(associations)
associations$terms <- row.names(associations)
associations$terms <- factor(associations$terms, levels = associations$terms)
                      
ggplot(associations, aes(y = terms)) + geom_point(aes(x = apologies), data = associations, size = 5) +
  theme_gdocs() + geom_text(aes(x = apologies, label = apologies), colour = "darkred", hjust = -.25, size = 8) +
  theme(text = element_text(size = 20), axis.title.y = element_blank())

                      
