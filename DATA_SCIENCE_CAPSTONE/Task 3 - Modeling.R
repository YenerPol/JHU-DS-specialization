# ---- read files ---- ----
setwd("C:/Users/gomez/Documents/datasciencecoursera/DATA_SCIENCE_CAPSTONE")
load("./data/output_task2.RData")
library(tidyverse)
library(tidytext)
# --- cuting data ----
words25000 <- tk1_noStop_stats[1:25000,]
# filter 25000 words
tk2 <-  tk2_noStop %>% 
        filter(word1 %in% words25000$word1) %>% 
        filter(word2 %in% words25000$word1)

num_row_tk2 <- nrow(tk2)

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





