setwd("C:/Users/gomez/Documents/datasciencecoursera/DATA_SCIENCE_CAPSTONE")
# (1) ---- 
## en_US.blogs.txt megabytes 
file.info("./final/en_US/en_US.blogs.txt")$size/10^6

# (2) ---- 
## The en_US.twitter.txt has how many lines of text?
length(readLines("./final/en_US/en_US.twitter.txt"))

# (3) ---- 
## What is the length of the longest line seen in any of the three en_US data sets?
library(LaF)
longest_line <- function(x){
        nl <- determine_nlines(x)
        long_line <- 0
        conection <- file(x, "r")
        for(i in 1:nl){
                n <- nchar(readLines(conection,1)) # readlines avanza 1 linea cada lectura
                if(n>long_line){long_line <- n}
        }
        close(conection)
        c(x,long_line)
}
files <- list.files("./final/en_US", full.names=TRUE)
i <- 0
longest_line(files[1])
longest_line(files[2]) # error ¿por que?
longest_line(files[3])

# (4) ---- 
##In the en_US twitter data set, if you divide the number of lines where the word "love" 
## (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, 
## about what do you get?
library(tidyverse)
# twitter <- read_file("./final/en_US/en_US.twitter.txt")  # todo en un character object
twitter <- readr::read_lines("./final/en_US/en_US.twitter.txt") # lee por lineas
n <- length(twitter)
love <- twitter %>% str_detect("love") %>% sum()
hate <- twitter %>% str_detect("hate") %>% sum()

# (5) ----
## The one tweet in the en_US twitter data set that matches the word "biostats" 
## says what?
twitter[twitter %>% str_detect("biostats")]
# (6) ----
## How many tweets have the exact characters "A computer once beat me at chess, 
## but it was no match for me at kickboxing". (I.e. the line matches those 
## characters exactly.)
exp <- "A computer once beat me at chess, but it was no match for me at kickboxing"
twitter %>% str_detect(exp) %>% sum()
