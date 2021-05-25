setwd("C:/Users/gomez/Documents/datasciencecoursera/DATA_SCIENCE_CAPSTONE")
#libraries ----
library(quanteda)
library(readtext)
library(stringi)
library(plotly)
library(tidyverse)

# Iniciating ----
path <- "./final/en_US/en_US"
filetype <- c(".blogs.txt", ".news.txt", ".twitter.txt")
blog <- paste0(path, filetype[1])
news <- paste0(path, filetype[2])
twitter <- paste0(path, filetype[3])

# Create corpus, each line is a document
corp_b <- corpus(stri_split_lines1(readtext(blog)))
corp_n <- corpus(stri_split_lines1(readtext(news)))
corp_t <- corpus(stri_split_lines1(readtext(twitter)))

# File size & Corpus size
sizes_corp <- c(format(object.size(corp_b), units="auto"), 
                format(object.size(corp_n), units="auto"), 
                format(object.size(corp_t), units="auto"))
sizes_file <- c(utils:::format.object_size(file.size(blog), "auto"), 
                utils:::format.object_size(file.size(news), "auto"), 
                utils:::format.object_size(file.size(twitter), "auto"))
sizes_all <- matrix(c(sizes_file, sizes_corp), nrow = 2, byrow = T, 
                    dimnames = list(c("File", "Corpus"), c("Blog", "News", "Twitter")))
sizes_all

# Exploratory Data Analysis ----

# Corpus Properties
token_b <- tokens(corp_b)
token_n <- tokens(corp_n)
token_t <- tokens(corp_t)

# The number of news, blog pieces, and tweets, as well as the longest and shortest texts in these datasets are shown below
df_b_len <- data.frame(len=sapply(token_b, length))
df_n_len <- data.frame(len=sapply(token_n, length))
df_t_len <- data.frame(len=sapply(token_t, length))

mtx_bstat <- matrix(nrow = 3, ncol = 3)
colnames(mtx_bstat) <- c("Number of Texts", "Longest Text", "Shortest Text")
rownames(mtx_bstat) <- c("Blog", "News", "Twitter")
mtx_bstat[1,] <- c(dim(df_b_len)[1], max(df_b_len), min(df_b_len))
mtx_bstat[2,] <- c(dim(df_n_len)[1], max(df_n_len), min(df_n_len))
mtx_bstat[3,] <- c(dim(df_t_len)[1], max(df_t_len), min(df_t_len))
mtx_bstat

head(corp_b[which(df_b_len==0)], 3)

# Text Length Distribution 

matrix(c(quantile(df_b_len$len, prob=c(0.5, 0.9, 0.99, 1)),
         quantile(df_n_len$len, prob=c(0.5, 0.9, 0.99, 1)),
         quantile(df_t_len$len, prob=c(0.5, 0.9, 0.99, 1))),
       byrow = T, nrow = 3, dimnames = list(c("Blog", "News", "Twitter"),
                                            c("50%", "90%", "99%", "100%")))

p_dist_b <- ggplot(df_b_len, aes(log10(len))) +
        geom_histogram(binwidth=density(log10(df_b_len$len))$bw, col="black", fill="red", alpha=0.8) +
        labs(x="Text Length (log10)", y="Text Count", title="Blog")
ggplotly(p_dist_b)

p_dist_n <- ggplot(df_n_len, aes(log10(len))) +
        geom_histogram(binwidth=density(log10(df_n_len$len))$bw, col="black", fill="green", alpha=0.8) +
        labs(x="Text Length (log10)", y="Text Count", title="News")
ggplotly(p_dist_n)

p_dist_t <- ggplot(df_t_len, aes(len)) +
        geom_histogram(binwidth=density(df_t_len$len)$bw, col="black", fill="blue", alpha=0.8) +
        labs(x="Text Length", y="Text Count", title="Tweet")
ggplotly(p_dist_t)

# N-gram Analysis ----
profanity <- read_csv("./data/BadWords.csv") %>% select(2) %>% rename("word" = "a55,")

for(i in 1:nrow(profanity)){
        profanity[i,] <- str_replace(profanity[i,], ",", "")
}

token_b <- tokens(token_b, remove_numbers=T, remove_punct=T, remove_symbols=T, remove_url=T) %>% tokens_remove(c(stopwords("en"), profanity$word))
token_n <- tokens(token_n, remove_numbers=T, remove_punct=T, remove_symbols=T, remove_url=T) %>% tokens_remove(c(stopwords("en"), profanity$word))
token_t <- tokens(token_t, remove_numbers=T, remove_punct=T, remove_symbols=T, remove_url=T) %>% tokens_remove(c(stopwords("en"), profanity$word))

# Top 20 Unigrams
dfm_b <- token_b %>% dfm()
dfm_t <- token_n %>% dfm()
dfm_n <- token_t %>% dfm()

dotchart(sort(topfeatures(dfm_b,20)), pch=19, xlab = "Count", main = "Top 20 Unigrams from Blog Corpus")
dotchart(sort(topfeatures(dfm_n,20)), pch=19, xlab = "Count", main = "Top 20 Unigrams from News Corpus")
dotchart(sort(topfeatures(dfm_t,20)), pch=19, xlab = "Count", main = "Top 20 Unigrams from News Corpus")

# Uncommon Unigrams
low_limit <- 1
freq_b_uni <- colSums(dfm_b)
sprintf("Tokens appear only once in Blog corpus: %.2f%%", (sum(freq_b_uni <= low_limit)/length(freq_b_uni))*100)

#Let’s take a look the proportion of uncommon words (which are tokens here) in different corpora and with different lower limit:

freq_n_uni <- colSums(dfm_n)
freq_t_uni <- colSums(dfm_t)
mtx_rare_terms_uni <- rbind(sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_b_uni<=x)/length(freq_b_uni))*100)),
                            sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_n_uni<=x)/length(freq_n_uni))*100)),
                            sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_t_uni<=x)/length(freq_t_uni))*100))) # sprintf() allows you to create strings as output using formatted data.
rownames(mtx_rare_terms_uni) <- c("Blog", "News", "Twitter")
colnames(mtx_rare_terms_uni) <- 1:4
mtx_rare_terms_uni

# Top 20 Bigrams
dfm_b_bi <- token_b %>% tokens_ngrams(2) %>% dfm()
dfm_n_bi <- token_n %>% tokens_ngrams(2) %>% dfm()
dfm_t_bi <- token_t %>% tokens_ngrams(2) %>% dfm()

dotchart(sort(topfeatures(dfm_b_bi,20)), pch=19, xlab = "Count", main = "Top 20 Bigrams from Blog Corpus")
dotchart(sort(topfeatures(dfm_n_bi,20)), pch=19, xlab = "Count", main = "Top 20 Bigrams from Blog Corpus")
dotchart(sort(topfeatures(dfm_t_bi,20)), pch=19, xlab = "Count", main = "Top 20 Bigrams from Blog Corpus")

# Uncommon Bigrams
freq_b_bi <- colSums(dfm_b_bi)
freq_n_bi <- colSums(dfm_n_bi)
freq_t_bi <- colSums(dfm_t_bi)
mtx_rare_terms_bi <- rbind(sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_b_bi<=x)/length(freq_b_bi))*100)),
                           sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_n_bi<=x)/length(freq_n_bi))*100)),
                           sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_t_bi<=x)/length(freq_t_bi))*100)))
rownames(mtx_rare_terms_bi) <- c("Blog", "News", "Twitter")
colnames(mtx_rare_terms_bi) <- 1:4
mtx_rare_terms_bi

# Top 20 Trigrams
dfm_b_tri <- token_b %>% tokens_ngrams(3) %>% dfm()
dfm_n_tri <- token_n %>% tokens_ngrams(3) %>% dfm()
dfm_t_tri <- token_t %>% tokens_ngrams(3) %>% dfm()

dotchart(sort(topfeatures(dfm_b_tri,20)), pch=19, xlab = "Count", main = "Top 20 Trigrams from Blog Corpus")
dotchart(sort(topfeatures(dfm_n_tri,20)), pch=19, xlab = "Count", main = "Top 20 Trigrams from Blog Corpus")
dotchart(sort(topfeatures(dfm_t_tri,20)), pch=19, xlab = "Count", main = "Top 20 Trigrams from Blog Corpus")

# Uncommon Trigrams
freq_b_tri <- colSums(dfm_b_tri)
freq_n_tri <- colSums(dfm_n_tri)
freq_t_tri <- colSums(dfm_t_tri)
mtx_rare_terms_tri <- rbind(sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_b_tri<=x)/length(freq_b_tri))*100)),
                            sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_n_tri<=x)/length(freq_n_tri))*100)),
                            sapply(1:4, function(x) sprintf("%.2f%%", (sum(freq_t_tri<=x)/length(freq_t_tri))*100)))
rownames(mtx_rare_terms_tri) <- c("Blog", "News", "Twitter")
colnames(mtx_rare_terms_tri) <- 1:4
mtx_rare_terms_tri

# Summary
# In this study, we find that the text datasets are:
        
# Large in size, which may pose difficulty for building model and prediction performance in terms of memory usage and speed.
# Containing some meaningless texts which are worthless for prediction.
# Filled with uncommon words, which can be discarded later when building model and does not affect the performance of prediction model a lot.