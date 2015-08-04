
library(readr)
library(stringr)
library(data.table)

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
        fread(input = paste(dir, 'combined_', x, '_all.csv', sep=''),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = T,
          verbose = F) })
}


#-----------------------------------------------------------
#   Create set of functions to use different model logic
#-----------------------------------------------------------
create_stub <- function(phrase, n, seperator='_') {
    #words <- str_split(phrase, seperator)
    
     vapply(str_split(phrase, seperator), function(x) {
         ifelse(length(x)-(n-2)>0, paste0(x[(length(x)-(n-2)):length(x)], collapse = '_'), '')
        
#         
#         if (length(x)-(n-2)>0) {
#             paste0(x[(length(x)-(n-2)):length(x)], collapse = '_')
#         }
#         else { '' }
    }, '')
}

get_ngram_match <- function(phrase, n, m=1, seperator='_') {
    #stub <- create_stub(phrase, n)
    
    matches <- grams[[n]][grepl(paste0('\\<', create_stub(phrase, n, seperator), '_', collapse = ''), grams[[n]]$token), ]
    
    if (nrow(matches)==0) { '' }
    else { 
        bestmatch <- matches[order(-total, -news_cnt, -blog_cnt)][1:m,]
        bestmatch$n <- n
        bestmatch$lastword <- vapply(bestmatch$token, function(x) str_split(x, '_')[[1]][n], '')
        bestmatch[!is.na(bestmatch$token),]
    }
}

model_predict_unique <- function(tdata, n, seperator='_') {
    vapply(tdata$phrase, function(x) {
        result <- get_ngram_match(x, n, 1, seperator)
        
        ifelse(length(result) == 1, '', result$lastword)
    }, '')
}

model_backoff <- function(tdata, seperator = '_') {
    vapply(tdata$phrase, function(x) {
        
        answer <- c('')
        
        #   Retrieve results for 4grams. If solid, use that
        result4 <- get_ngram_match(x, 4, 10, seperator)
        
        if (length(result4)==7) {
            if (result4[1,]$total > 5) {
                answer <- result4[1,]$lastword
            }
        }
        
        #   If not 4grams, retrieve results for 3 grams. Look for high frequency
        #       and perhaps a match back against 4grams?
        if (answer=='') {
            result3 <- get_ngram_match(x, 3, 20)
            
            if (class(result3)[1]=='data.table') {
                if (result3[1,c('total')] > 10) {
                    answer <- result3[1,]$lastword
                }
            }
        }
         
        #   If not trigrams, retrieve results for 2 grams
        if (answer=='') {
            result2 <- get_ngram_match(x, 2, 1)
            
            if (class(result2)[1]=='data.table') {
                answer <- result2[1,]$lastword
            }
        }
        
        answer
    }, '')
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
    


#   Pass traindata into 4gram model
traindata$prediction4 <- model_predict_unique(traindata, 4)
traindata$prediction3 <- model_predict_unique(traindata, 3)
traindata$prediction2 <- model_predict_unique(traindata, 2)
traindata$prediction <- model_backoff(traindata)


write.csv(as.data.frame(traindata), file = paste(dir, 'train_results.csv', sep=''))
traindata
str(as.data.frame(traindata))
str(traindata)



model_predict_unique(traindata[1:5], 4)







test <- fread(input = paste(dir,'combined_1.csv', sep=""),
      sep = ",",
      nrows = -1,
      header = T,
      stringsAsFactors = TRUE,
      verbose = T)

str(test)

test$token <- as.factor(test$token)

create_stub("big_shot_coach_nate_mcmillan_said_with_a", 4)


length(get_ngram_match('whitecaps_1_crew', n=4, m=15))


head(traindata)


model_predict_unique(traindata[1:10,], n=4)

model_backoff(quiz)


results <- get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=4, m=15)

rbind(get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=4, m=20),
      get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=3, m=20),
      get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=2, m=20))









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
