
#-----------------------------
#   Data load functions
#-----------------------------
load_grams <- function(gram_dir, mincount=0) {
    lapply(1:4, function(x) {
        dt <- fread(input = paste0(gram_dir, 'combined_', x, '.csv'),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = F,
          verbose = F) 
        
        if (mincount > 0) {
            dt <- dt[total > mincount]
        }
        
        setkey(dt, stub, total, news_cnt, blog_cnt)
        
    })
}

load_text_data <- function(filename = 'news', filetype = 'train', nrows = -1) {
    docs <- fread(input = paste0(dir, filename, '_', filetype, '.csv'),
            nrows = nrows,
            header = T,
            stringsAsFactors = F,
            verbose = F)
    
    setnames(x = docs, old = paste0(filetype, '_doc'), new = 'document')
    
    docs
}


#-----------------------------
#   Model Utility functions
#-----------------------------
get_tomatch <- function(words, n) {
    #   Match last word only for n=1,2
    #   Otherwise match last n-1 words
    #       n=3, match last 2 words
    #       n=4, match last 3 words
    firstword <- max(1, length(words) - ifelse(n<3, 0, n-2))
    
    paste(words[firstword:length(words)], collapse = '_')
}

get_logprob <- function(matches, preceding, lambda = 1) {
    if (nrow(matches) > 0)
    { 
        prob <- log((matches$total / sum(matches$total)) * lambda, base = 2)
#         logprob_news <- ifelse(sum(matches$news_cnt)==0,0,log((matches$news_cnt / sum(matches$news_cnt)) * lambda, base = 2))
#         logprob_blog <- ifelse(sum(matches$blog_cnt)==0,0,log((matches$blog_cnt / sum(matches$blog_cnt)) * lambda, base = 2))
#         logprob_twit <- ifelse(sum(matches$twit_cnt)==0,0,log((matches$twit_cnt / sum(matches$twit_cnt)) * lambda, base = 2))
        prediction <- matches$lastword
    } else {
        prob <- log(.00001, base = 2)
#         logprob_news <- log(.00001, base = 2)
#         logprob_blog <- log(.00001, base = 2)
#         logprob_twit <- log(.00001, base = 2)
        prediction <- ''
    }
    
    data.table(preceding=preceding, to_predict = c(''), guess=prediction, logprob=prob, keep.rownames = F)
}

calculate_perplexity <- function(logprob, m) {
    2 ^ (-1 * (logprob / m))
}


#------------------------
#       Models
#------------------------
model_gram <- function (words, param1, param2) {
    n <- param1
    
    tomatch <- get_tomatch(words, n)
    
    #   Retrieve ngram matches and calculate the probability for the best choice
    matches <- grams[[n]][stub==tomatch,][order(-total, -news_cnt, -blog_cnt)]

    logprobs <- get_logprob(matches, tomatch)
    logprobs
}

model_backoff_simple <- function(words, param1, param2) {
    #   Get last three words and try to match against 4grams
    tomatch4 <- get_tomatch(words, 4)
    
    matches4 <- grams[[4]][stub==tomatch4, ][order(-total, -news_cnt, -blog_cnt)]
    
    if (nrow(matches4) > 0) {
        logprobs <- get_logprob(matches4, tomatch4)
        logprobs
    } else {
        
        #   If no match, try 3grams
        tomatch3 <- get_tomatch(words, 3)
        matches3 <- grams[[3]][stub==tomatch3, ][order(-total, -news_cnt, -blog_cnt)]
        if (nrow(matches3) > 0) {
            logprobs <- get_logprob(matches3, tomatch3, lambda = .4)
            logprobs
        } else {
            
            #   If no match, try 2grams
            tomatch2 <- get_tomatch(words, 2)
            matches2 <- grams[[2]][stub==tomatch2, ][order(-total, -news_cnt, -blog_cnt)]
            logprobs <- get_logprob(matches2, tomatch2, lambda = (.4*.4))
            logprobs
        }
    }
}

model_backoff_stupid <- function(words, param1, param2) {
    stupid_results <- lapply(4:2, function(x) {
        tomatch <- get_tomatch(words, x)

        matches <- grams[[x]][stub==tomatch, ][order(-total, -news_cnt, -blog_cnt)]
        if (nrow(matches) > 0) {
            logprobs <- get_logprob(matches, tomatch)
            logprobs
        }
    })
    
    all <- rbindlist(stupid_results, fill = T)
    if(nrow(all) > 0) { all[order(-logprob)] }
}


#------------------------
#   Execution functions
#------------------------
process_documents_old <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {
    #   Use stringi function directly, will be a small bit faster
    words <- stri_split_fixed(docs, separator)[[1]]
    
    #   Loop through each word and send it and the previous words to supplied model
    results <- lapply(1:length(words), function(x) {
        output <- model(words[max(1,(x-3)):x], param1, param2)[1,]
        if(length(output)>0) { output$to_predict <- words[x+1] }
        output
    })
    
    #   Return results as data frame to simplify usage
    solution <- data.frame()
    rbind(solution, do.call(rbind, results))
}

run_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {
    results <- process_documents_old(docs, model, ' ', param1, param2)
    
    #   Shift data so the predicted word is next to the actual word
    accuracy <- data.table(results[, .(preceding, to_predict)], results[, guess], results[, logprob])
    setnames(accuracy, old = c('V2', 'V3'), new = c('guess', 'logprob'))

    accuracy
}

evaluate_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0, n = 0) {

    #   SET TIMER
    ptm <- proc.time()
    
    accuracy <- run_model(docs, model, separator, param1, param2)
    accuracy <- accuracy[!is.na(accuracy$to_predict),]
    
    doc_count <- nrow(docs)
    word_count <- nrow(accuracy)
    accuracy_rate <- sum(accuracy[,to_predict] == accuracy[,guess]) / nrow(accuracy)
    perplexity <- calculate_perplexity(sum(accuracy$logprob), nrow(accuracy))
    
    #   STOP TIMER
    runtime <- (proc.time() - ptm)[c('elapsed')]
    
    data.frame(accuracy_rate, perplexity, doc_count, word_count, n, runtime, as.character(Sys.time()))
}

trim <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
}

clean_input <- function(typing) {
    x <- trim(tolower(typing))
    
    gsub("[^[:alpha:][:space:]]", "", x)
}

predict_word <- function(typing) {
    submit_text <- data.table(clean_input(typing))
    
    predictions <- model_backoff_stupid(stri_split_fixed(submit_text, ' ')[[1]])

    if (length(predictions)>0) {
        predictions$prob <- predictions[, 2^predictions$logprob]

        predictions[, probability:=sum(prob), by=guess]
        unique(predictions[order(-probability),.(guess, probability)])[1:15]
    }
}


#predict_word('this waycvfdfdf')
#predict_word('on one side of ')
#predict_word("I just can't ")

#   model_backoff_stupid(stri_split_fixed('on one side of an', ' ')[[1]])
#   model_backoff_stupid(stri_split_fixed("I just can't", ' ')[[1]])
# get_tomatch(clean_input("I just can't"), 2)

#gsub(pattern = "[^[:alpha:][:space:]]", replacement = '', x = "I just can't")

# x[1,guess]

grams = load_grams('data/', mincount=3)

# 
# grams[[1]][stub=='this']
# 
# grams[[1]][stub==get_tomatch('this', 1), ][order(-total, -news_cnt, -blog_cnt)]
# 
# logprobs <- get_logprob(matches2, tomatch2, lambda = (.4*.4))











# process_document <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {
#     #   Clean up docs
#     #     docs <- tolower(docs)
#     #     docs <- gsub(docs, pattern = "[[:punct:]]|( )$", replacement = '')
#     
#     lapply(docs, function(x) {
#         #   Use stringi function directly, will be a small bit faster
#         words <- stri_split_fixed(x, separator)
#         
#         #   Fix the SOD/EOD tokens. Even though we're going to delete the SOD rows,
#         #   we still want to predict off of it. (And potentially match the first word.)
#         #     words[grep(pattern = '##d#', x = words)] <- '##d#'
#         #     words[grep(pattern = '"#d#', x = words)] <- '#d#'
#         
#         #   Loop through each word and send it and the previous words to supplied model
#         results <- lapply(1:length(words), function(x) {
#             output <- model(words[max(1,(x-3)):x], param1, param2)
#             output$to_predict <- words[x+1]
#             output
#         })
#         
#         #   Return results as data frame to simplify usage
#         #solution <- data.frame()
#         #rbind(solution, do.call(rbind, results))
#     })
# }
# 
# process_documents <- function(docs, model, separator=' ', param1, param2) {
#     lapply(docs[,document,with=F], function(x) {
#         words <- stri_split_fixed(tolower(x), pattern = separator)
#         
#         #   Loop through each word and send it and the previous words to supplied model
#         lapply(words, function(y) {
#             lapply(1:(length(y)), function(z) {
#                 output <- model(y[max(1,(z-3)):z], param1, param2)
#                 output$to_predict <- words[z+1]
#                 output
#             })
#             
#             #             solution <- data.frame()
#             #             rbind(solution, do.call(rbind, output))
#             
#         })
#     })
#     
#     
#     #    rbindlist(results)
# }

