#  Tasks to accomplish 

## Exploratory analysis 
## Understand frequencies of words and word pairs

# ---- Setup ----
setwd("C:/Repositorios Github/JHU-specialization/JHU-DS-specialization/DATA_SCIENCE_CAPSTONE")
library(tidyverse)
library(tidytext)


# ---- reading the output from task 1 ----
load("./data/output_task1.RData")

token1 <- token1 %>% count(word1, sort = TRUE)
occurrences <- sum(token1$n)
token1 <- token1 %>% mutate(freq = n/occurrences)

token1 %>% summary()

# ---- plot 1 with the 20 most frequent words in vocabulary ----

# Here we can see that we have a really huge vocabulary, there are more than a 
# hundred thousand words! But, in the other hand, the summary of this vocabulary 
# shows that 75% of the words are repeated just 5 times or less. 
# Cut the data by frecuency could help making our model faster and lighther. 

# The most frequent words are:
        
token1 %>% .[1:20,] %>% 
        mutate(name = fct_reorder(word1, n)) %>%
        ggplot( aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
        coord_flip() + ylab("Counts") +
        xlab("") + ggtitle("20 Most Frequent Words") +
        theme_gray()

# selecting words with count >3
threshold <- 3
token1 <- token1 %>% filter(n > threshold) 
occurrences <- sum(token1$n)
token1 <- token1 %>% mutate(freq = n/occurrences)

token1 %>% summary()

head(token1)

# this function gets 2-gram and 3-gram data (1 word per column) and 
# remove words that doesn't appear in the vocabulary 
clean_func <- function(dat, vocabulary){
        if(ncol(dat) == 2){
                dat %>% filter((word1 %in% vocabulary$word1) | (word2 %in% vocabulary$word1))
        }else if(ncol(dat) == 3){
                dat %>% filter((word1 %in% vocabulary$word1) | (word2 %in% vocabulary$word1)  | (word3 %in% vocabulary$word1))
        }
}

# Filtering token2 y token3
token2 <- clean_func(token2, token1)
token3 <- clean_func(token3, token1)

head(token3)

# ---- plot 2 with the 20 most frequent bigrams ----

# To count Bigrams and Trigrams, it's necesesary to combine each row 
# in a single character column:  

token2 <- token2 %>% 
        unite(bigram, word1, word2, sep = " ") %>% 
        count(bigram, sort = T)

token2 %>% .[1:20,] %>% 
        mutate(name = fct_reorder(bigram, n)) %>%
        ggplot(aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
        coord_flip() + 
        xlab("") + ylab("Counts") + ggtitle("20 Most Frequent Bigrams") +
        theme_gray()

# ---- plot 3 with the 20 most frequent trigrams ----

token3 <- token3 %>% 
        unite(trigram, word1, word2, word3, sep = " ") %>% 
        count(trigram, sort = T)

token3 %>% .[1:20,] %>% 
        mutate(name = fct_reorder(trigram, n)) %>%
        ggplot(aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
        coord_flip() + 
        xlab("") + ylab("Counts") + ggtitle("20 Most Frequent Trigrams") +
        theme_gray()

# An interesting question is - How many unique words do we need in a frequency
# sorted dictionary to cover 50% of all word instances in the language? 90%? Lets see:

token1 %>% 
        ggplot(aes(x=1:nrow(token1), y=cumsum(freq))) + 
        geom_jitter() + xlab("Word rank") + 
        ylab("Freq Cumulative sum") +
        geom_hline(yintercept=0.95, color = "blue") +
        geom_hline(yintercept=0.75, color = "pink") +
        geom_hline(yintercept=0.50, color = "red") +
        theme_gray()

cum_sum <- cumsum(token1$freq) 
token1$cumsum <- cum_sum

cum_050 <- sum(cum_sum <= 0.50)
cum_075 <- sum(cum_sum <= 0.75)
cum_095 <- sum(cum_sum <= 0.95)

# ---- Zipf’s law. pag 44 tidytext ---- 

token1 %>%
        mutate(Rank = row_number()) %>% 
        ggplot(aes(x = Rank, y = freq)) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
        scale_x_log10() +
        scale_y_log10() + 
        geom_abline(intercept = -0.8, slope = -1, color = "gray50", linetype = 2) +
        labs(title = "Zipf’s law", x = "log(Rank)", y = "log(Freq)")

# ---- save final tables ----
token1 <-  token1 %>% mutate(Rank = row_number())
save(token1, token2, token3,  file="./data/output_task2.RData")

