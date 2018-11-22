suppressPackageStartupMessages({
  library(tidyverse);library(stringr);library(dplyr);library(tidyr);library(tidytext)
}) # Package load out 

#Loading the ngrams
fourgrams <- readRDS(gzcon(url("https://github.com/Bladouceur/captstone-data/blob/master/fourgrams.rds?raw=true")))
trigrams  <- readRDS(gzcon(url("https://github.com/Bladouceur/captstone-data/blob/master/trigrams.rds?raw=true")))
bigrams <- readRDS(gzcon(url("https://github.com/Bladouceur/captstone-data/blob/master/bigrams.rds?raw=true")))

# For bigram, trigrams and fourgrams, this function takes the input, filter the ngram based on the word and picks
# the top choice by frequency
bigram <- function(input_words){
  num <- length(input_words)
  filter(bigrams, 
         word1==input_words[num]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "NA", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(trigrams, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

fourgram <- function(input_words){
  num <- length(input_words)
  filter(fourgrams, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}


prediction <- function(input){
  input <- data_frame(text = input)
  replace_reg <- "[^[:alpha:][:space:]]*" # clean up fun
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word"))) # to vector
  input_words <- tolower(input_words) #remove caps because people use too many of them
  out <- ifelse(input_count == 1, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), fourgram(input_words))) # select function to use
  # Output
  return(out)
}
