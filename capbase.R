#install.packages("readr")
#install.packages('data.table')
#install.packages('Matrix')
#install.packages('quanteda')
#install.packages('stringi')

library(readr)
library(data.table)
library(Matrix)
library(quanteda)
library(stringi)


#---------------------------------------
#   Initialize variables
#---------------------------------------
filenames <- c("news", "blogs", "twitter")
files <- c("files/news_final.txt",
           "files/blogs_final.txt",
           "files/twitter_final.txt")


#--------------------------------
#   Execution Parameters
#--------------------------------
#   set to -1 to import all records
reccount = 200
samplenum <- 90
seed = 3500

# reccount = 2000
# samplenum <- 90
# seed = 234

# reccount = -1
# samplenum <- 97
# seed = 680

dir <- paste("files/", samplenum, "_", seed, "_", reccount, "/", sep="")
