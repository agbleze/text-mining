######## tEXT Mining

##set options
options(stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL", "C")

## load libraries
library(stringi)
library(stringr)
library(qdap)


library(readr)
text.df <- read_csv("/Volumes/SAMSUNG/text_mining-master 2/oct_delta.csv")
View(text.df)
nchar(head(text.df$text))
mean(nchar(text.df$text))
subset.doc <- subset(text.df, nchar(text.df$text)>0)
sub("thanks", "thank you", text.df[1,5], ignore.case = TRUE)
sub("PLS", "PLEASE", text.df[1:5, 5], ignore.case = F)

fake.text <- 'R text mining is good but text mining in python is also'
sub('text mining', 'tm', fake.text, ignore.case = F)
gsub('text mining', 'tm', fake.text, ignore.case = F)
gsub('&amp', '', text.df[5, 5])
