# ---- read files ---- ----
setwd("C:/Repositorios Github/JHU-specialization/JHU-DS-specialization/DATA_SCIENCE_CAPSTONE")
load("./data/output_task2.RData")
library(tidyverse)

# --- cuting data ----

sum(token1$cumsum <= 0.95) #11k palabras explican el 95% 
token1 <- token1[1:15000,]

# n_total_tk2 <- sum(token2$n) 
# n_total_tk3 <- sum(token3$n)

# token2 <- token2 %>% mutate(freq = n/n_total_tk2)
# token3 <- token3 %>% mutate(freq = n/n_total_tk3)


# filtrar vocabulario y separar palabras en columnas ----------------------

library(tidytext)

bigram <- token2$bigram %>% as_tibble() %>% separate(value, c("word_1", "word_0"), sep = " ")
trigram <- token3$trigram %>% as_tibble() %>% separate(value, c("word_2", "word_1", "word_0"), sep = " ")

token2$word_1 <- bigram$word_1
token2$word_0 <- bigram$word_0

token3$word_2 <- trigram$word_2
token3$word_1 <- trigram$word_1
token3$word_0 <- trigram$word_0

rm(bigram, trigram)

token2 <- token2 %>% filter(word_1 %in% token1$word1) %>% filter(word_0 %in% token1$word1) 

token3 <- token3 %>% filter(word_2 %in% token1$word1) %>% filter(word_1 %in% token1$word1) %>% filter(word_0 %in% token1$word1) 


# 3-gram Model ------------------------------------------------------------

lista_3_gram <- token3 %>% split(token3$word_2)
lista_3_gram <- lapply(lista_3_gram, function(x) x %>% split(.[]$word_1) )

for (i in 1:length(lista_3_gram)) {
        for (j in 1:length(lista_3_gram[[i]])) {
                lista_3_gram[[i]][[j]] <- lista_3_gram[[i]][[j]][1:3,] 
        }
}
lista_3_gram <- lapply( X = lista_3_gram, FUN = lapply, "[", c("word_0", "n") ) # borrar trigram cuando este seguro



# 2-gram Model special ----------------------------------------------------

lista_2_gram_spec <- token3 %>% select(word_1, word_0, n)
lista_2_gram_spec <- lista_2_gram_spec %>% split(.[]$word_1)
lista_2_gram_spec <- lapply(lista_2_gram_spec, function(x) x %>% group_by(word_0) %>% summarize(sum(n)) %>% arrange(desc(`sum(n)`)) %>% .[1:3,] )     
