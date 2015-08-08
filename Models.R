source(file="capbase.r")



#-----------------------------------------------------------
#   Initialize data before calling various model functions
#   This will centralize data loading and setup and help minimize 
#   testing overhead
#-----------------------------------------------------------
load_grams <- function(dir) {
    lapply(1:4, function(x) {
        dt <- fread(input = paste(dir, 'combined_', x, '_all.csv', sep=''),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = T,
          verbose = F) 
        
        #setkey(dt, stub, total, news_cnt)
    })
}

get_ngram_match <- function(phrase, n, m=1, seperator='_') {
    matches <- grams[[n]][stub==create_stub(phrase, n, seperator), ]
    
    if (nrow(matches)>=0)
    { 
        bestmatch <- matches[order(-total, -news_cnt, -blog_cnt)][1:m,]
        bestmatch$n <- n
        bestmatch[!is.na(bestmatch$stub),]
    }
}

model_predict_unique <- function(tdata, n, seperator='_') {
    vapply(tdata$phrase, function(x) {
        result <- get_ngram_match(x, n, 1, seperator)
        
        ifelse(length(result) == 1, '', as.character(result$lastword))
    }, '', USE.NAMES = F)
}

model_backoff <- function(tdata, seperator = '_') {
    lapply(tdata$phrase, function(x) {
        
        #   Retrieve results for 4grams. If solid, use that
        results <- rbind(
            get_ngram_match(x, 4, 10, seperator)[n>5],
            get_ngram_match(x, 3, 10, seperator)[n>10],
            get_ngram_match(x, 2, 10, seperator))
        
        as.character(results[order(-n, -total)][1,lastword])
    } )
}


#------------------------
#   Process Driver
#------------------------
grams <- load_grams(dir)

traindata <- fread(input = paste(dir, 'news_train.csv', sep=''),
              nrows = -1,
              header = T,
              stringsAsFactors = F,
              verbose = F)
    


#   Pass traindata into 4gram model
traindata$prediction4 <- model_predict_unique(traindata, 4)
traindata$prediction3 <- model_predict_unique(traindata, 3)
traindata$prediction2 <- model_predict_unique(traindata, 2)
traindata$prediction <- model_backoff(traindata)


write.csv(as.data.frame(traindata), file = paste(dir, 'train_results.csv', sep=''))





model_predict_unique(traindata[1:50], 4)

t<-model_predict_unique(traindata, 4)










str(traindata)

str(grams[[4]])



model_predict_unique(traindata[2:5], 4)


lapply(traindata[1:10,]$phrase, function(x) {
    get_ngram_match(x, n=4, m=15)
})

get_ngram_match('a_second_straight', n=4, m=15)
b<-get_ngram_match('said_with_a', n=3, m=15)
c<-get_ngram_match('said_with_a', n=2, m=15)
d<-get_ngram_match('said_with_a', n=1, m=15)

rbind(a,b,c, d)


grams[[1]][stub=='000s',]


phrase <-'said_with_a'
n <- 2
m = 1 
seperator='_'

n <- 1
create_stub(phrase, n, seperator)
n <- 2
create_stub(phrase, n, seperator)
n <- 3
create_stub(phrase, n, seperator)
n <- 4
create_stub(phrase, n, seperator)

model_predict_unique(traindata[1:100,], 4)


create_stub(traindata[1:10,]$phrase, 4, '_')

get_ngram_match(traindata[1:10,]$phrase, 4)

grams[[4]]




create_stub(traindata[1,]$phrase, n, seperator)



length(str_split(phrase, seperator)[[1]])
str_split(phrase, seperator)[[1]][2:3]

    matches <- grams[[n]][stub==create_stub(phrase, n, seperator), ]
    
    if (nrow(matches)>=0)
    { 
        bestmatch <- matches[order(-total, -news_cnt, -blog_cnt)][1:m,]
        bestmatch$n <- n
        bestmatch[!is.na(bestmatch$stub),]
    }








create_stub('said', 1, '_')
create_stub('said_with_a', 3, '_')
create_stub('said_with_a', 2, '_')
create_stub('said', 1, '_')
create_stub('said', 1, '_')



str_split('said', '_')

ifelse(length('said')-(n-2)>0, paste0(x[(length(x)-(n-2)):length(x)], collapse = '_'), phrase)



vapply(str_split('said', '_'), function(x) {
    ifelse(length(x)-(n-1)>0, paste0(x[(length(x)-(n-1)):length(x)], collapse = '_'), phrase)
}, '')


rbind(b,c)

rbind(data.table(), a,b,c)


model_predict_unique('said_with_a', n=4)



model_backoff(traindata[1:10])

x<-traindata[1,phrase]
#   Retrieve results for 4grams. If solid, use that
results <- rbind(
    get_ngram_match(x, 4, 10, seperator)[n>5],
    get_ngram_match(x, 3, 10, seperator)[n>10],
    get_ngram_match(x, 2, 10, seperator))

as.character(results[order(-n, -total)][1,lastword])

tables()


model_backoff(quiz1)


results <- get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=4, m=15)

rbind(get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=4, m=20),
      get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=3, m=20),
      get_ngram_match('big_shot_coach_nate_mcmillan_said_with_a', n=2, m=20))











test[[3]][order(-total, -news_cnt, -blog_cnt)][1,]


str_split(testdata[1:2,]$stub, '_')



View(testdata[grep('__', testdata$stub),])

testdata$clean <- gsub("(_)\\1+", "\\1", testdata$stub)

test_doc$stub <- gsub("(_)\\1+", "\\1", test_doc$stub)
