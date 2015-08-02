
library(readr)
library(stringr)

#---------------------------------------
#   Initialize variables
#---------------------------------------
filenames <- c("news", "blogs", "twitter")
files <- c("files/news_final.txt",
           "files/blogs_final.txt",
           "files/twitter_final.txt")




#-----------------------------------------------------------
#   Initialize data before calling various model functions
#   This will centralize data loading and setup and help minimize 
#   testing overhead
#-----------------------------------------------------------
load_grams <- function(dir) {
    #lapply(1:4, function(n) read_lines(paste(dir, 'combined_', n, '.csv', sep=''), n_max = -1))
    
    lapply(1:4, function(x) {
        fread(input = paste(dir, 'combined_', x, '.csv', sep=''),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = T,
          verbose = F) })
}




#-----------------------------------------------------------
#   Create set of functions to use different model logic
#-----------------------------------------------------------
create_stub <- function(tdata, n) {
    words <- str_split(tdata$phrase, '_')
    
    vapply(words, function(x) {
        if (length(x)-(n-2)>0) {
            paste(x[(length(x)-(n-2)):length(x)], collapse = '_')
        }
        else { '' }
    }, '')
}

get_ngram_match <- function(stub, n) {
    matches <- grams[[n]][grepl(paste0('\\<', stub, collapse = ''), grams[[n]]$token), ]
    
    if (nrow(matches)==0) { '' }
    else { 
        bestmatch <- matches[order(-total, -news_cnt, -blog_cnt)][1,]
        
        str_split(bestmatch$token, '_')[[1]][n]
    }
}

model_predict4 <- function(tdata) {

    tdata$stub <- create_stub(tdata, 4)

    lapply(tdata$stub, function(x) {
        get_ngram_match(x, 4)
    })
}


#------------------------
#   Process Driver
#------------------------
grams <- load_grams(dir)
    
traindata <- fread(input = paste(dir, 'news_train.csv', sep=''),
              nrows = -1,
              header = T,
              stringsAsFactors = T,
              verbose = F)
    

#   Pass traindata into model
traindata$prediction <- model_predict4(traindata)







testdata$stub <- create_stub(testdata, 4)

test <- lapply(testdata[1:4,]$stub, function(x) {
    matches <- grams[[4]][grepl(paste0('\\<',x,collapse = ''), grams[[4]]$token), ]
    
    if (nrow(matches)==0) { '' }
    else { 
        
        bestmatch <- matches[order(-total, -news_cnt, -blog_cnt)][1,]
        #bestmatch$token
        str_split(bestmatch$token, '_')[[1]][4]
        }
    }    
)


test[[3]][order(-total, -news_cnt, -blog_cnt)][1,]


str_split(testdata[1:2,]$stub, '_')



View(testdata[grep('__', testdata$stub),])

testdata$clean <- gsub("(_)\\1+", "\\1", testdata$stub)

test_doc$stub <- gsub("(_)\\1+", "\\1", test_doc$stub)
