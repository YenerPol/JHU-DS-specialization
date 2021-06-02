# ---- Tasks to accomplish ----
## Exploratory analysis 
## Understand frequencies of words and word pairs

# ---- Setup ----
setwd("C:/Repositorios Github/JHU-specialization/JHU-DS-specialization/DATA_SCIENCE_CAPSTONE")
library(tidyverse)
library(tidytext)


# ---- reading the output from task 1 ----
load("./data/output_task1.RData")

# ---- my stop words ----

# --- stop words ---
# (my_stop_words) El primer grupo de palabras a eliminar es el grupo de palabras 
# que aparecen pocas veces en el data set. n <= 3. Ademas se agregan palabras 
# manualmente que no significan nada. 

my_stop_words <- token1_task_1 %>% 
        count(word1, sort = T) %>% 
        filter(n<=3) %>% 
        select(word1)

# tomadas de una inspeccion manual
my_stop_words <- rbind(my_stop_words, 
                       data.frame("word1" = c("rt","lol","im","oh","ce","ðÿ","ya",
                                              "wow","la","tv","re","yo", "ah","ugh",
                                              "tho","thx","yay","em","da","fb","î",
                                              "wtf","xd","etc","mt","aw","co","dr",
                                              "de","nba","pp","php","mc", "bc")))
# esto por ahora se queda 
stop <- stopwords::stopwords()

# ---- plot 1 - top 20 frequent words mantaining stop_words ----
gplot_1 <- token1_task_1 %>% 
        count(word1, sort = T) %>% 
        filter(!(word1 %in% my_stop_words$word1)) %>%
        .[1:20,] %>% 
        mutate(name = fct_reorder(word1, n)) %>%
        ggplot( aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") + ggtitle("Most Frequent words") +
        theme_bw()

# ---- plot 2 - top 20 frequent words removing stop_words ----
gplot_2 <- token1_task_1 %>% 
        count(word1, sort = T) %>% 
        filter(!(word1 %in% stop)) %>%
        filter(!(word1 %in% my_stop_words$word1)) %>%
        .[1:20,] %>% 
        mutate(name = fct_reorder(word1, n)) %>%
        ggplot( aes(x=name, y=n) ) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() + xlab("") + 
        ggtitle("Most Frequent words - No stop words") + theme_bw()

gridExtra::grid.arrange(gplot_1, gplot_2, nrow = 1)

# ---- new cleaning ----
## tal vez quiera filtrar algo mas, por eso en una funcion
clean_func <- function(data,bad1,bad2){
        if(ncol(data)==1){
                data %>% filter(!(word1 %in% bad1$word1))
                #data %>% filter(!(word1 %in% bad1$word1 | word1 %in% bad2))
        }else if(ncol(data)==2){
                data %>% filter(!(word1 %in% bad1$word1 | word2 %in% bad1$word1 )) 
                #data %>% filter(!(word1 %in% bad1$word1 | word1 %in% bad2))
        }else if(ncol(data)==3){
                data %>% filter(!(word1 %in% bad1$word1 | word2 %in% bad1$word1 | word3 %in% bad1$word1)) 
                #data %>% filter(!(word1 %in% bad1$word1 | word1 %in% bad2))
        }
}

tk1_noStop <- clean_func(token1_task_1, my_stop_words, stop)
head(tk1_noStop)
tk2_noStop <- clean_func(token2_task_1, my_stop_words, stop)
head(tk2_noStop)
tk3_noStop <- clean_func(token3_task_1, my_stop_words, stop)
head(tk3_noStop)

num_rows <- nrow(tk1_noStop)

rm(token1_task_1, token2_task_1, token3_task_1)
# ---- tk1 stats -----
tk1_noStop_stats <- tk1_noStop %>%
        count(word1, sort = T) %>%
        rowid_to_column("rank") %>% 
        mutate(freq = n/num_rows) 

head(tk1_noStop_stats)

# ---- Zipf’s law. pag 44 tidytext ---- 
tk1_noStop_stats %>%
        ggplot(aes(x = rank, y = freq)) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
        scale_x_log10() +
        scale_y_log10() + 
        geom_abline(intercept = -0.8, slope = -1, color = "gray50", linetype = 2) +
        labs(title = "Zipf’s law", x = "log(Rank)", y = "log(Freq)")
        
# ---- plot 3 - top 20 frequent 2-grams mantaining stop_words ----
gplot3 <- token2_task_1 %>% 
        unite(bigram, word1, word2, sep = " ") %>% 
        count(bigram, sort = T) %>% 
        .[1:20,] %>% 
        mutate(name = fct_reorder(bigram, n)) %>%
        ggplot( aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") + ggtitle("Most Frequent bigrams. Keeping stop words.") +
        theme_minimal()

# ---- plot 4 - top 20 frequent 2-grams removing stop_words ----
gplot4 <- tk2_noStop %>% 
        unite(bigram, word1, word2, sep = " ") %>% 
        count(bigram, sort = T) %>% 
        .[1:20,] %>% 
        mutate(name = fct_reorder(bigram, n)) %>%
        ggplot( aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") + ggtitle("Most Frequent bigrams. No stop words.") +
        theme_minimal()

# ---- plot 5 - top 20 frequent 3-grams mantaining stop_words ----
gplot5 <- token3_task_1 %>% 
        unite(trigram, word1, word2, word3, sep = " ") %>% 
        count(trigram, sort = T) %>% 
        .[1:20,] %>% 
        mutate(name = fct_reorder(trigram, n)) %>%
        ggplot( aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") + ggtitle("Most Frequent trigrams. stop words.") +
        theme_bw()

# ---- plot 6 - top 20 frequent 2-grams removing stop_words ----
gplot6 <- tk3_noStop %>% 
        unite(trigram, word1, word2, word3, sep = " ") %>% 
        count(trigram, sort = T) %>% 
        .[1:20,] %>% 
        mutate(name = fct_reorder(trigram, n)) %>%
        ggplot(aes(x=name, y=n)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() +
        xlab("") + ggtitle("Most Frequent trigrams. No stop words.") +
        theme_bw()

dev.new()
gridExtra::grid.arrange(gplot1, gplot3, gplot5, gplot2, gplot4, gplot6,
                        nrow =2)
dev.off()

# ---- plot density ----
explain <- function(data, threshold){
        x <- 0
        cont <- 1
        while (x < threshold) {
              x <- x + data$freq[cont]
              cont <- cont + 1
        }
        cont
}
explain(tk1_noStop_stats, 0.50) #897
explain(tk1_noStop_stats, 0.75) #3707
explain(tk1_noStop_stats, 0.90) #10952
explain(tk1_noStop_stats, 0.95) #18694
explain(tk1_noStop_stats, 0.975) #20232

# ---- save final tables ----
save(tk1_noStop_stats, tk2_noStop, tk3_noStop,  file="./data/output_task2.RData")

