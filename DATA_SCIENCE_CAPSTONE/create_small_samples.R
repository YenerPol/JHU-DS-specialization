setwd("C:/Users/gomez/Documents/datasciencecoursera/DATA_SCIENCE_CAPSTONE")
files <- list.files("./final/en_US", full.names=TRUE)
library(tidyverse)
library(LaF)

# Downloading data ----

datapath <- "./data"
filename <- "./data/rawdata.zip"
if (!file.exists(datapath)) { dir.create(datapath) }
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileURL, filename)
}
unzip(filename)

# samples ----
small_sample <- function(x, name = "sample_blogs.txt"){
        nl <- determine_nlines(x)
        conection <- file(x, "r")
        samp <- data.frame(sample_lines(x, n = round(nl/20))) # samples of 5% of the original size
        write_tsv(samp, path = paste("./data/", name, sep = ""))
        close(conection)
}
small_sample(files[1], name = "sample.en_US.blogs.txt")
small_sample(files[2], name = "sample.en_US.news.txt")
small_sample(files[3], name = "sample.en_US.twitter.txt")

# borrar <- read.delim2("./data/sample.en_US.twitter.txt") # lectura perfecta

