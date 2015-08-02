#install.packages("readr")

#install.packages("readr")
#install.packages("ngram")
#install.packages("RWeka")
#install.packages("tm")
#install.packages("plyr")

library(readr)
library(ngram)
library(RWeka)
library(tm)
library(plyr)
library(ggplot2)

row.sample <- function(dta, rep = 20) {
    dta <- as.data.frame(dta) # for single variables
    dta[sample(1:nrow(dta), rep, replace=FALSE), ] 
} 



#tr -cd '[:alnum:][:punct:][:space:]' < en_US.twitter.txt > outputfile.txt


#   set to -1 to import all records
n <- -1


#---------------------------------------
#   Apply tokenization (Before or after ngram?)
#   Determine strategy
#       Remove punctuation?
#       Whitespace?
#       Remove uppercase?
#       Stopwords
#       Stemming?
#       Remove profanity?
#---------------------------------------



#---------------------------------------
#   Import sample of each dataset
#   "Keep in mind that the first n rows of the data set may not be representative."
#---------------------------------------

news <- as.data.frame(read_lines("files/en_US.news.txt", n_max = n))
blogs <- as.data.frame(read_lines("files/en_US.blogs.txt", n_max = n))
twitter <- as.data.frame(read_lines("files/en_US.twitter.txt", n_max = n))

corp_news <- VCorpus(DataframeSource(x = news))
corp_blogs <- VCorpus(DataframeSource(x = blogs))
corp_twit <- VCorpus(DataframeSource(x = twitter))




#  inspect(corp_news[1:3])

#  as.character(corp_news[[3]])

#  lapply(corp_twit[1:10], as.character)

#   meta(corp_twit[[15]])
#   meta(corp_twit)


# newdocs[[1]][8]
# 
# #   Correct right leaning single quotes
# newdocs <- lapply(docs, function(x) apply(x, function(y) gsub(pattern = 'â€™', replacement = "'", y)))
# 
# newdocs[[1]][8]
# 
# class().gsub(pattern = "â€™", replacement = 'x')
# gsub(pattern = "â€™", replacement = "'", x = docs[[1]][8])
# 


#---------------------------------------------------
#   Create DocumentTermMatrix and inspect items
#---------------------------------------------------
dtm_news <- DocumentTermMatrix(corp_news)
dtm_blogs <- DocumentTermMatrix(corp_blogs)
dtm_twit <- DocumentTermMatrix(corp_twit)


inspect(dtm_news)

str(inspect(dtm_news[310:320, 1124:1129]))

findFreqTerms(dtm_news, 20)

class(dtm_news)

class(removeSparseTerms(dtm_news, .98))

freq <- as.data.frame(colSums(as.matrix(removeSparseTerms(dtm_news, .98))))
colnames(freq) <- c('cnt')

class(freq[order(-freq$cnt), , drop=F])

hist(x = freq[order(-freq$cnt), , drop=F][,1], breaks = seq(min(freq)-1, max(freq)+20, 10))

hist(x = freq[order(-freq$cnt), , drop=F][freq$cnt,1], breaks = seq(0, 100, 5))


#---------------------------------------------------
#   Create n-grams of each sample + combined set
#       1, 2, 3, 4 n-grams?
#---------------------------------------------------


#--------------------------------------------------
#   Create basic summaries of samples + combined
#   Include data tables and histograms
#   1)  Word counts
#   2)  Line counts
#--------------------------------------------------




#----------------------------------------------------------
#   How many unique words do you need in a frequency
#   sorted dictionary to cover 50% of all word instances
#   in the language? 90%?
#----------------------------------------------------------


#--------------------------------------------------
#   Import corpus of total english words and run
#   comparisons of % of words found in datasets
#   (Might want to use full corpa for this analysis)
#--------------------------------------------------


#-------------------------------------------------------------------------
#   Identify words uniquely represented in each corpus and not in others
#-------------------------------------------------------------------------



