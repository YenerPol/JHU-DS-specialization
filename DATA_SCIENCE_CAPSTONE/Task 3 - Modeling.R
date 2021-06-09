# ---- read files ---- ----
setwd("C:/Users/gomez/Documents/datasciencecoursera/DATA_SCIENCE_CAPSTONE")
load("./data/output_task2.RData")
library(tidyverse)

# --- cuting data ----

cumsum95 <- sum(token1$cumsum <= 0.95)
token1 <- token1[1:15000,]

# filtering top 15000 words
library(tidytext)

bigram <- token2$bigram %>% as_tibble() %>% separate(value, c("word1", "word2"), sep = " ")
trigram <- token3$trigram %>% as_tibble() %>% separate(value, c("word1", "word2", "word3"), sep = " ")

token2$word1 <- bigram$word1
token2$word2 <- bigram$word2

token3$word1 <- trigram$word1
token3$word2 <- trigram$word2
token3$word3 <- trigram$word3

token2 <- token2 %>% 
        filter(word1 %in% token1$word1) %>% 
        filter(word2 %in% token1$word1)

token3 <- token3 %>% 
        filter(word1 %in% token1$word1) %>% 
        filter(word2 %in% token1$word1) %>% 
        filter(word1 %in% token1$word1)  



# counting bigrams
tk2_count <- tk2 %>% 
        unite(bigram, word1, word2, sep = " ") %>% 
        count(bigram, sort = T) %>% 
        separate(bigram, c("word1", "word2"), sep = " " )
n <- dim(words25000)[1]
df <- matrix(0, nrow = n, ncol =n) # atento al peso
colnames(df) <- words25000$word1
rownames(df) <- words25000$word1

llenar <- function(){
        z <- nrow(tk2_count)
        for(i in 1:z){
                df[tk2_count$word1[i], tk2_count$word2[i]] <- tk2_count$n[i]
        }
        
}

df <- llenar()





