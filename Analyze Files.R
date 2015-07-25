#install.packages("readr")
#install.packages('data.table')
#install.packages('Matrix')
#install.packages('quanteda')

library(readr)
library(data.table)
library(Matrix)
library(quanteda)



#---------------------------------------
#   Initialize variables
#---------------------------------------
#   set to -1 to import all records
remove_stopwards <- 1

filenames <- c("news", "blogs", "twitter")
files <- c("files/news_final.txt",
           "files/blogs_final.txt",
           "files/twitter_final.txt")


#-------------------------------
#   Execution Code
#-------------------------------
ngram <- 4

for(i in 1:3) {
    create_freq_file(filenum=i, reccount=-1, n=ngram)
}

test <- combine_files(reccount=-1, n=ngram)

combined <- Reduce(function(x,y) {merge(x,y,by="V1",all=T)}, test)

setnames(combined, old= c('V1', 'V2.x', 'V2.y', 'V2'), new = c('token', 'news_cnt', 'blog_cnt', 'twit_cnt'))

write.csv(combined, file=paste("files/combined_", ngram, ".csv", sep=""), quote = F, row.names = F)

#rm(combined)
#rm(test)



#-------------------------------
#   Function Declarations
#-------------------------------
create_freq_file <- function(filenum, reccount, n) {
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
    doc <- tolower(read_lines(files[filenum], n_max = reccount))

    
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
    corp <- corpus(doc, enc="UTF-8")
    
    rm(doc)
    gc()
    
    
    #----------------------------------------------------------
    #   Create n-gram DocumentFrequencyMatrix
    #----------------------------------------------------------
    tokens <- tokenize(corp, ngrams = n, removeNumbers = T, removeSeparators = T)
    dfm <- dfm(tokens, ignoredFeatures = words_to_remove, verbose=F)
    freq <- as.data.table(docfreq(dfm), keep.rownames=T)
    write.csv(freq, file=paste("files/",filenames[filenum],"_", n, ".csv", sep=""), quote = F)
}


combine_files <- function(reccount, n) {
    freqs <- list()
    
    for (i in 1:3) {
        freqs[[i]] <- fread(input = paste("files/",filenames[i],"_", n, ".csv", sep=""),
                            sep = ",",
                            nrows = reccount,
                            header = T,
                            stringsAsFactors = T,
                            verbose = F)[,-1, with=F] # Remove first column which is just the rowname from the write.csv step
            
    }
    
    freqs
}








