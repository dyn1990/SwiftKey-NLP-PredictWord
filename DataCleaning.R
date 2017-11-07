###############################
##### load and clean data #####
###############################
library(gridExtra)
library(ggplot2)
library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordnet)
library(SnowballC)
library(koRpus)
data(stop_words)
rm(list = ls())

local <- "C:\\Users\\Dyn\\Documents\\YAD_JohnHopkins_DS\\data\\capstone\\Coursera-SwiftKey"
setwd(paste(local, "\\script", sep = ""))
source("tmFunctions.R")

filename_twitter <-  paste(local, "\\final\\en_US\\en_US.twitter.txt", sep = "")
filename_blog <-  paste(local,"\\final\\en_US\\en_US.blogs.txt", sep = "")
filename_news <-  paste(local,"\\final\\en_US\\en_US.news.txt", sep = "")

twitter <- read.table(filename_twitter, sep = "\n", quote = "",
                      stringsAsFactors = FALSE, encoding = "UTF-8", skipNul = TRUE)
news <- read.table(filename_news, sep = "\n", quote = "",
                   stringsAsFactors = FALSE, encoding = "UTF-8", skipNul = TRUE)
blog <- read.table(filename_blog, sep = "\n", quote = "",
                   stringsAsFactors = FALSE, encoding = "UTF-8", skipNul = TRUE)

############################################################################
##### taking only 10 percent of each corpus and combine different text #####
############################################################################
set.seed(1111)
cleanTwitter <- cleanText(sample_n(twitter, floor(dim(twitter)[1]/10)), rs = TRUE)
cleanNews <- cleanText(sample_n(news, floor(dim(news)[1]/10)), rs = TRUE)
cleanBlog <- cleanText(sample_n(blog, floor(dim(blog)[1]/10)), rs = TRUE)
cText <- rbind(cleanTwitter, cleanNews, cleanBlog)

rm(twitter); rm(news); rm(blog)
rm(cleanTwitter); rm(cleanNews); rm(cleanBlog)

saveRDS(cText, "cleanDataset.rds")




