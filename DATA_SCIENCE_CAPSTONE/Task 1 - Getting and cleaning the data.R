# ---- Tasks to accomplish ---- ----

## Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
## Writing a function that takes a file as input and returns a tokenized version of it.
## Profanity filtering - removing profanity and other words you do not want to predict.

# ---- read files ---- ----

setwd("C:/Repositorios Github/JHU-specialization/JHU-DS-specialization/DATA_SCIENCE_CAPSTONE")
## files path
path <- list(blogs = "./data/sample.en_US.blogs.txt",
                news = "./data/sample.en_US.news.txt",
                twitter = "./data/sample.en_US.twitter.txt")

library(tidyverse)

blogs <- read_tsv(path$blogs) %>% rename("text"="sample_lines.x..n...round.nl.20..")
news <- read_tsv(path$news) %>% rename("text"="sample_lines.x..n...round.nl.20..")
twitter <- read_tsv(path$twitter) %>% rename("text"="sample_lines.x..n...round.nl.20..")
rm(path)
# ---- tokenization ---- ----
library(tidytext)
## this function receive raw data
token_func <- function(rawdata){
        # Input : raw data. tibble n x 1. 1 sentence per row. 
        # Output : tokenized Version of sentences. list of tibbles.
        #           list[[1]] is a tibble m x 1 (1-gram)
        #           list[[2]] is a tibble m x 2 (2-gram)
        #           list[[3]] is a tibble m x 3 (3-gram)
        token_w4 <- rawdata %>% 
                unnest_tokens(word, text, token = "ngrams", n=3, to_lower = T)
        token_w4 <- token_w4 %>% separate(word, c("word1", "word2", "word3", "word4"), sep = " ")
        tokens_list <- list(token_w4[,1],token_w4[,1:2],token_w4[,1:3],token_w4[,1:4])
}
## each list contains tokens x1 word, x2 word, x3 word
blogs_list <- token_func(blogs)  
news_list <- token_func(news)
twitter_list <- token_func(twitter)

token1 <- rbind(blogs_list[[1]], news_list[[1]], twitter_list[[1]])
token2 <- rbind(blogs_list[[2]], news_list[[2]], twitter_list[[2]])
token3 <- rbind(blogs_list[[3]], news_list[[3]], twitter_list[[3]])

rm(blogs_list, news_list, twitter_list)

# ---- Profanity filtering function ---- ----
library(lexicon) # lexicon::profanity_alvarez list of profanity words
profan_words <- tibble(profanity_alvarez) %>% rename("word"="profanity_alvarez")
#badwords <- read_csv("./data/BadWords.csv") %>% select(2) %>% rename("word" = "a55,")
# for(i in 1:nrow(badwords)){
#         badwords[i,] <- str_replace(badwords[i,], ",", "")
# }
#profan <- full_join(profan_words, badwords)

# separate and filter function 

separar_filtrar <- function(data, profan){
        if(ncol(data) == 3){
                ngrams_filtered <- data %>%
                        filter(!(is.na(word1) | is.na(word2) | is.na(word3) )) %>% 
                        filter(!((nchar(word1) == 1) | (nchar(word2) == 1) | (nchar(word3) == 1) ) ) %>% 
                        filter(!(str_detect(word1, "[0-9]") | str_detect(word2, "[0-9]") | str_detect(word3, "[0-9]") ) ) %>% 
                        filter(!(str_detect(word1, "â") | str_detect(word2, "â") | str_detect(word3, "â") ) ) %>%
                        filter(!(word1 %in% profan$word | word2 %in% profan$word | word3 %in% profan$word)) %>% 
                        filter(!(str_detect(word1, "hah") | str_detect(word2, "hah") | str_detect(word3, "hah") ) )
        }else if(ncol(data) == 2){
                ngrams_filtered <- data %>%
                        filter(!(is.na(word1) | is.na(word2) )) %>% 
                        filter(!((nchar(word1) == 1) | (nchar(word2) == 1) ) ) %>% 
                        filter(!(str_detect(word1, "[0-9]") | str_detect(word2, "[0-9]") ) ) %>% 
                        filter(!(str_detect(word1, "â") | str_detect(word2, "â") ) ) %>% 
                        filter(!(word1 %in% profan$word | word2 %in% profan$word )) %>%
                        filter(!(str_detect(word1, "hah") | str_detect(word2, "hah") ) )            
        }else if(ncol(data) == 1){
                ngrams_filtered <- data %>%
                        filter(!(is.na(word1))) %>% 
                        filter(!((nchar(word1) == 1) ) ) %>% 
                        filter(!(str_detect(word1, "[0-9]") )) %>% 
                        filter(!(str_detect(word1, "â") ) ) %>% 
                        filter(!(word1 %in% profan$word )) %>%
                        filter(!(str_detect(word1, "hah") ) )
        }
        ngrams_filtered
}

# ---- filtering data ---- ----
       
token1 <- separar_filtrar(token1, profan_words)
token2 <- separar_filtrar(token2, profan_words)
token3 <- separar_filtrar(token3, profan_words)

rm(token1,token2,token3)

# ---- save data ----

#puede guardar multiples archivos OJO!
save(token1, token2, token3, file="./data/output_task1.RData") 



