#install.packages("readr")
#install.packages('data.table')
#install.packages('Matrix')
#install.packages('quanteda')

library(readr)
library(data.table)
library(Matrix)
library(quanteda)
library(stringr)
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

dir <- paste("files/", samplenum, "_", seed, "_", reccount, "/", sep="")
masterdir <- 'files/mastergrams'

#------------------------------------------------
#   Import profanity list / set words_to_remove
#------------------------------------------------
remove_stopwords <- 1

manual_words <- c('[', ']')
if (!exists('profanity')) {
    profanity <- read_lines("files/profanity.txt")
}

if (remove_stopwords == 1) {
    words_to_remove <- c(profanity, stopwords("english")) 
} else {
    words_to_remove <- profanity
}


#-----------------------------------------------------------
#   Create set of functions to use different model logic
#-----------------------------------------------------------
create_stub <- function(phrase, n, seperator='_') {
    vapply(str_split(phrase, seperator), function(x) {
        ifelse(n==1, x[length(x)], 
               ifelse(length(x) < (n-1), '', paste0(x[(length(x)-(n-2)):length(x)], collapse = '_')))
    }, '')
}

trim <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
}
