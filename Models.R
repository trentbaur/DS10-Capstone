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
          stringsAsFactors = F,
          verbose = F) 
        
        setkey(dt, stub, total, news_cnt)
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
    vapply(tdata$phrase, function(x) {
        
        #   Retrieve results for 4grams. If solid, use that
        results <- rbind(
            get_ngram_match(x, 4, 10, seperator)[n>5],
            get_ngram_match(x, 3, 10, seperator)[n>10],
            get_ngram_match(x, 2, 10, seperator))
        
        as.character(results[order(-n, -total)][1,lastword])
    }, '', USE.NAMES = F )
}


#------------------------
#   Process Driver
#------------------------
run_models <- function(filename = 'news', filetype = 'train', nrows = -1) {
    td <- fread(input = paste(dir, filename, '_', filetype, '.csv', sep=''),
                       nrows = nrows,
                       header = T,
                       stringsAsFactors = F,
                       verbose = F)

    #   Pass traindata into various models and store answer
    td$prediction4 <- model_predict_unique(td, 4)
    td$prediction3 <- model_predict_unique(td, 3)
    td$prediction2 <- model_predict_unique(td, 2)
    td$backoff <- model_backoff(td)

    write.table(lapply(td, unlist), file = paste(dir, filename, '_', filetype, '_results.csv', sep=''), quote = F, append = F, sep = ',')
    
    td
}

grams <- load_grams(dir)

for (i in 1:3) {
    results <- run_models(filenames[i], 'train', nrows=-1)
}






model_backoff(traindata[1:100])




