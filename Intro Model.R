#install.packages("readr")
#install.packages('data.table')
#install.packages('Matrix')
#install.packages('quanteda')

library(readr)
library(data.table)
library(Matrix)
library(quanteda)

quiz <- c(  "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
            "You're the reason why I smile everyday. Can you follow me please? It would mean the",
            "Hey sunshine, can you follow me and make me the",
            "Very early observations on the Bills game: Offense still struggling but the",
            "Go on a romantic date at the",
            "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
            "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
            "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
            "'Be grateful for the good times and keep the faith during the",
            "If this isn't the cutest thing you've ever seen, then you must be")

setwd("D:/Projects/Coursera/10 - Capstone")

#---------------------------------------
#   Initialize variables
#---------------------------------------
#   set to -1 to import all records
n <- -1
remove_stopwards <- 1

filenames <- c("news", "blogs", "twitter")
files <- c("files/news_final.txt",
           "files/blogs_final.txt",
           "files/twitter_final.txt")


#---------------------------------------------------------
#   Import sample of each dataset
#   "Keep in mind that the first n rows of the data set
#       may not be representative."
#   Prepocessing that has been completed:
#       Change slashes to spaces (athletes/competitors)
#       Change punctuation to spaces (Hmmm...I think)
#       Remove everything but alphanumeric and spaces
#   After import, make all text lower case
#---------------------------------------------------------
if (!exists('docs')) {
    docs <- lapply(files, function(x) read_lines(x, n_max = n))
    docs <- lapply(docs, function(x) tolower(x))
}


#---------------------------------
#   Import profanity list
#---------------------------------
if (!exists('profanity')) {
    profanity <- read_lines("files/profanity.txt")
}

if (remove_stopwards == 1) {
    words_to_remove <- c(profanity, stopwords("english")) 
    } else {
    words_to_remove <- profanity
}


#----------------------------------------------------------
#   Create corpus via quanteda package
#----------------------------------------------------------
corps <- lapply(docs, function(x) corpus(x, enc="UTF-8"))
maincorp <- corps[[1]] + corps[[2]] + corps[[3]]

rm(corps)
rm(docs)
gc()


#----------------------------------------------------------
#   Create 1:4-gram DocumentFrequencyMatrix
#----------------------------------------------------------
# tok1 <- tokenize(maincorp, ngrams = 1, removeNumbers = T, removeSeparators = T)
# dfm1 <- dfm(tok1, ignoredFeatures = words_to_remove, verbose=F)
# as.data.table(colSums(as.matrix(dfm1)), keep.rownames = T)
# total1 <- sum(as.matrix(dfm1))
freq1 <- as.data.table(docfreq(dfm1), keep.rownames=T)

tok <- lapply(1:4, function(x) {
                tokenize(maincorp,
                        ngrams = x,
                        removeNumbers = T, 
                        removeSeparators = T) } )

rm(maincorp)
gc()

dfms <- lapply(tok, function(x) {
                dfm(x,
                    ignoredFeatures = stopwords("english"),
                    verbose=F) } )


rm(tok)
gc()


totals <- lapply(dfms, function(x) sum(as.matrix(x)))

freqs <- lapply(dfms, function(x) as.data.table(colSums(as.matrix(x)), keep.rownames = T))

rm(dfms)
gc()

#------------------------------------------------------------------------------------
#   Calculate log probability of each n-gram
#   If need be, maybe this can be optimized with calculating against unique counts
#       or using index keys
#------------------------------------------------------------------------------------
for(i in 1:length(freqs)) {
    freqs[[i]]$prob <- log(freqs[[i]]$V2 / totals[[i]])
}




#---------------------------------------------
#   Model Functions
#---------------------------------------------
quiz2 <- function(quiz_num) {
    #   Break down quiz text into final few words
    words <- strsplit(quiz[quiz_num], " ")[[1]]
    
    stub3 <- words[seq((length(words)-2), length(words))]
    stub2 <- words[seq((length(words)-1), length(words))]
    
    #    Build stub with underscores to match against
    match3 <- paste('^', paste0(stub3, collapse='_'), sep="")
    match2 <- paste('^', paste0(stub2, collapse='_'), sep="")
    
    #   Look for matchs in various ngrams
    rbind(freqs[[4]][like(V1, match3)],
          freqs[[3]][like(V1, match2)])
}

# 
# quiz2(1)
# 
# 
# 
# 
# 
# 
# lapply(dfms, function(x) topfeatures(x, n=20))
# 
# freqs[[2]][1:100,]
# 
# freqs[[3]][order(-V2)][1:100]
# 
# 
# 
# 
# paste(c("st", "nd", "rd"), collapse = "_")
# 
# 
# apply(q, function(x) paste0(x, sep='_'))
# 
# len <- length(strsplit("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", " ")[[1]])
# 

# 
# 
# x<-1
# setorder(freqs[[x]], -V2)
# head(freqs[[x]], 30)
# 
# str(maincorp)

