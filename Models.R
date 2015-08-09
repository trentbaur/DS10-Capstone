source(file="capbase.r")

#-----------------------------------------------------------
#   Initialize data before calling various model functions
#   This will centralize data loading and setup and help minimize 
#   testing overhead
#-----------------------------------------------------------
load_grams <- function(master=0) {
    lapply(1:4, function(x) {
        dt <- fread(input = paste(ifelse(master, masterdir, dir), 'combined_', x, '.csv', sep=''),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = F,
          verbose = F) 
        
        setkey(dt, stub, total, news_cnt, blog_cnt)
    })
}

get_tomatch <- function(words, n) {
    #   Match last word only for n=1,2
    #   Otherwise match last n-1 words
    #       n=3, match last 2 words
    #       n=4, match last 3 words
    firstword <- max(1, length(words) - ifelse(n<3, 0, n-1))
    
    paste(words[firstword:length(words)], collapse = '_')
}

get_logprob <- function(matches, lambda = 1) {
    if (nrow(matches) > 0)
    { 
        prob <- log((matches[1,]$total / sum(matches$total)) * lambda)
        prediction <- matches[1,]$lastword
    } else {
        prob <- log(.00001)
        prediction <- ''
    }
    
    data.table(pred=prediction, logprob=prob, keep.rownames = F)
}


#------------------------
#       Models
#------------------------
model_gram <- function (words, param1, param2) {
    n <- param1
    
    tomatch <- get_tomatch(words, n)
    
    #   Retrieve ngram matches and calculate the probability for the best choice
    matches <- grams[[n]][stub==tomatch,][order(-total, -news_cnt, -blog_cnt)]

    logprobs <- get_logprob(matches)
    logprobs
}

model_backoff_simple <- function(words, param1, param2) {
    #   Get last three words and try to match against 4grams
    tomatch4 <- get_tomatch(words, 4)
    matches4 <- grams[[4]][stub==tomatch4, ][order(-total, -news_cnt, -blog_cnt)]
    if (nrow(matches4) > 0) {
        get_logprob(matches4)
    } else {
        
        #   If no match, try 3grams
        tomatch3 <- get_tomatch(words, 3)
        matches3 <- grams[[3]][stub==tomatch3, ][order(-total, -news_cnt, -blog_cnt)]
        if (nrow(matches3) > 0) {
            get_logprob(matches3, lambda = .4)
        } else {
            
            #   If no match, try 2grams
            tomatch2 <- get_tomatch(words, 2)
            matches2 <- grams[[2]][stub==tomatch2, ][order(-total, -news_cnt, -blog_cnt)]
            get_logprob(matches2, lambda = (.4*.4))
        }
    }
}


#------------------------
#   Execution functions
#------------------------
process_document <- function(text, separator=' ', model, param1 = 0, param2 = 0) {
    words <- stri_split_fixed(text, separator)[[1]]
    
#    results <- data.frame()
    
#     for(i in 1:length(words)) {
#         results <- rbind(results, model(words[i:(i+3)], param1, param2))
#     }
#     rbind(model(words[[1]][1:4], param1, param2),
#     model(words[[1]][2:5], param1, param2),
#     model(words[[1]][3:6], param1, param2),
#     model(words[[1]][4:7], param1, param2))
    
    results <- lapply(1:length(words), function(x) {
        model(words[max(1,(x-3)):x], param1, param2)
    })
    
    #   Return 
    solution <- data.frame()
    rbind(solution, do.call(rbind, results))
    #solution
    #   Transpose results    
    #results
}



test <- process_document('Initialize data before calling various model functions', ' ', model_gram, 2)
sum(test$logprob)


str(test)

class(t(test))

undebug(process_document)
undebug(model_gram)
undebug(get_logprob)


test <- process_document('Initialize data before calling various model functions', ' ', model_gram, 2)
class(test[1,1])


process_document('Initialize data before calling various model functions', ' ', model_backoff_simple)


lapply(test, function(x) { sum(x$prob)} )
class((test$prob))


library(stringi)
str_split('Initialize data before calling various model functions', ' ')
stri_split_fixed('Initialize data before calling various model functions', ' ')[[1]]








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

grams <- load_grams(master=0)

for (i in 1:3) {
    results <- run_models(filenames[i], 'train', nrows=-1)
}






model_backoff(traindata[1:100])




